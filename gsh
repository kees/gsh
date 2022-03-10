#!/usr/bin/perl

=head1 NAME

B<gsh> - Global Shell run commands in parallel to multiple machines

=head1 SYNOPSIS

gsh [OPTIONS] SYSTEMS CMD...

 SYSTEMS is a combination of ghost macros.  See ghosts(1).
 CMD is the command to run

 -h, --help            Display full help
 -H, --manpage         Display full man page
 -a, --alive SECONDS   Skip hosts that cannot be pinged after SECONDS secs
 -b, --banner          Add one-line banner before each host output
 -d, --debug           Turn on exeuction debugging reports
 -g, --ghosts          Use specified ghosts configuration file
 -i, --immediate       Show output when ready, do not wait to sort by host
 -l, --user USER       SSH's to the host as user USER
 -n, --open-stdin      Leaves stdin open when running (scary!)
 -o, --self-remote     Run command over SSH even for local host
 -p, --no-host-prefix  Does not prefix output lines with the host name
 -r, --run-locally     Run commands locally (replaces some $var for you)
 -s, --show-commands   Displays the command before the output report
 -t, --tabulate        Align host output and prefixes nicely
 -C, --copy DIR        Copies specified entries to remote DIR on each host
 -L, --force-user USER Force USER, superseding users from ghosts file
 -V, --version         Report the version and exit
 -X, --extra-ssh       Supply extra arguments to ssh

=head1 DESCRIPTION

The idea behind this tool originally came from wanting to do something
on each machine in our network.  Existing scripts would serially go to
each machine run the command, wait for it to finish, and continue to the
next machine.  There was no reason why this couldn't be done in parallel.

The problems, however, were many.  First of all, the output from finishing
parallel jobs needs to be buffered in such a way that different machines
wouldn't output their results on top of eachother.

A final bit was added because it was nice to have output alphabetical
rather than first-done, first-seen.  The result is a parallel job
spawner that displays output from the machines alphabetically, as soon
as it is available.  If "alpha" take longer than "zebra", there will
be no output past "alpha" until it is finished.  As soon as "alpha"
is finished, though, everyone's output is printed.

Sending a SIGUSR1 to gsh(1) will cause it to report which machines are
still pending.  (Effectively turns on B<--debug> for one cycle.)

You can use the B<GSH_HOSTS> environment variable to supply the default
location of the ghosts file, but B<-g> will always supersede it.
If unset, the default value is C</etc/ghosts>.

=head1 OPTIONS

=over 8

=item B<-h>, B<--help>

Displays this detailed help.

=item B<-H>, B<--manpage>

Displays the complete manual page (prettier if C<perldoc> is installed).

=item B<-a>, B<--alive> SECONDS

Issue a ping with SECONDS timeout to each host, skipping those not responding.

=item B<-b>, B<--banner>

Emit an 80-column one-line banner at the top of each host output, listing
the host name.

This can be used along with B<-p> to limit visual clutter in the output
and yet be able to distinguish easily each host output.

=item B<-d>, B<--debug>

Turns on debugging.  A regular report of pending hosts is create, and PIDs
are show as commands are executed and reaped.

=item B<-g>, B<--ghosts> CONFIG_FILE

Uses the provided ghosts configuration file, instead of /etc/ghosts. This
means /etc/ghosts will not be read, at all.

=item B<-i>, B<--immediate>

As soon as a host finishes running its command, display the output.

The default behaviour is to wait so that we can sort the output
alphabetically by host name.

=item B<-l>, B<--user> USER

SSH as a user USER on the remote machines, but without superseding any
USER specified in the ghosts file for a given host.  Use B<-L> to force.

=item B<-n>, B<--open-stdin>

Leave stdin open when SSH'ing.  This can cause hangs and other strange
situations, but can be useful in uncommon situations where you need to
pipe input to all of the child processes.

=item B<-o>, B<--self-remote>

Normally, if the local host running gsh is listed among the hosts to SSH
to, gsh will just run the command locally instead of attempting to SSH
back to the local machine.  If you want gsh to SSH to the local machine
anyway, turn this option on.

=item B<-p>, B<--no-host-prefix>

Turns off the prefixing of hostnames to the output reports.

=item B<-r>, B<--run-locally>

Instead of SSH'ing to hosts, run the commands locally.  The string '$host'
will be replaced with the name of the current host, '$port' with the remote
SSH port and '$user' with the remote user.  For example:

  gsh -r all 'echo $user@$host:$port'

=item B<-s>, B<--show-command>

Displays the command being run before the output report for each host.

=item B<-t>, B<--tabulate>

Align start of command output nicely after the initial host prefix.  This
requires large terminal width, greater than 80 columns or line wrapping will
add visual clutter.

=item B<-C>, B<--copy-to> DIR

Instead of running SSH, SCP will be used to copy all the specified
files to the remote DIR on each host.

For instance:

	gsh -C /tmp all file1 file2 /path/file3 /path/dir1

would create a copy of all the specified files or directories (recursively)
in the C</tmp> directory of all hosts.

A report simply lists success or failure, but does not give detail about
which files could not be copied remotely (insufficient permission on
the remote host, I/O error, etc.).

Permissions and original timestamps are preserved by SCP if possible.

=item B<-L>, B<--force-user> USER

SSH as a user USER on the remote machines, superseding any USER
configuration specified in the ghosts file.  See also B<-l>.

=item B<-V>, B<--version>

Displays the version information and exits.

=item B<-X>, B<--extra-ssh>

Supplies extra arguments to the B<ssh> command being run, which can be used
to configure port forwarding.

For instance:

	gsh -X '-R 2222:githost.example.com:22'

could be used on hosts where we wish to synchronize a git repository held
on the githost.example.com machine, which would be normally not accessible
by the remote host.  It would then be able to define its repository as:

	ssh://localhost:2222/path/to/repository.git

thereby allowing synchronization operations through the forwarded port.

Note that the B<-X> argument is a quoted string, so that it is read as one
big value, but it will be internally split on white spaces to create the
extra arguments for B<ssh>, making quotation of spaces within that argument
impossible.

=back

=cut

# Thanks to:
#	- whoever originally gave this idea to: Mike Murphy
#	- Mike Murphy for actually implementing this at Motorola
#	- Paul Holcomb for various fixes
#	- Nick Asvos for finding an out-of-memory bug and reporting it

use strict;
use warnings;

our $NAME = "gsh";
our $VERSION = "1.2.0";

use SystemManagement::Ghosts;
use POSIX "sys_wait_h";
use File::Temp qw/ tempdir /;
use Getopt::Long qw(:config no_ignore_case bundling require_order);
use Pod::Usage;
use List::Util qw(max);

our $opt_help = 0;
our $opt_manpage = 0;
our $opt_alive = 0;
our $opt_banner = 0;
our $opt_copy_to = "";
our $opt_debug = 0;
our $opt_ghosts = $ENV{GSH_HOSTS} || "/etc/ghosts";
our $opt_immediate = 0;
our $opt_tabulate = 0;
our $opt_no_host_prefix = 0;
our $opt_show_command = 0;
our $opt_open_stdin = 0;
our $opt_user = 0;
our $opt_force_user = 0;
our $opt_run_locally = 0;
our $opt_self_remote = 0;
our $opt_version = 0;
our $opt_extra_ssh = "";

use constant {
	EXIT_OK => 0,
	EXIT_OPTIONS => 1,
	EXIT_NO_COMMAND => 2,
	EXIT_ERRORED => 3,
	EXIT_QUIT => 4,
};

GetOptions(
	"help|h",
	"manpage|H",
	"alive|a=i",
	"banner|b",
	"debug|d",
	"ghosts|g=s",
	"immediate|i",
	"no-host-prefix|p",
	"show-command|s",
	"tabulate|t",
	"open-stdin|n",
	"user|l=s",
	"run-locally|r",
	"self-remote|o",
	"copy-to|C=s",
	"force-user|L=s",
	"version|V",
	"extra-ssh|X=s",
)
or pod2usage(-verbose => 0, -exitstatus => EXIT_OPTIONS);

pod2usage(-verbose => 2, -exitstatus => EXIT_OK) if $opt_manpage;

if ($opt_help) {
	my $out = \*STDOUT;
	if (-t STDOUT) {
		my $pager = $ENV{PAGER} || "more";
		$out = \*PAGER if open(PAGER, "| $pager");
	}
	pod2usage(-verbose => 1, -exitstatus => EXIT_OK, -output => $out)
}

Version() if $opt_version;

my $me = $0;
$me =~ s|.*/(.*)|$1|;
my $systype = shift(@ARGV);		# get name representing set of hosts
my @cmd = @ARGV;				# remaining args constitute the command

pod2usage(-verbose => 0, -exitstatus => EXIT_NO_COMMAND) unless @cmd;

SystemManagement::Ghosts::Load($opt_ghosts);
my @BACKBONES = SystemManagement::Ghosts::Objects($systype);
die "$me  no matching hosts found in $opt_ghosts\n" unless @BACKBONES;

my $TMP = tempdir( CLEANUP => 1 );

my $showpid = undef;	# shows PIDs
my $signals = undef;	# uses the child signal handler

$| = 1;

# Global process trackers
my %output;
my %pidlist;
my %showlist; # report header, per host
my $viewwaiting = 0; # Should we report who we're waiting for?

# set up signal handlers: we must die gracefully and attempt to kill children
$SIG{'QUIT'} = 'quit';			# install signal handler for SIGQUIT
$SIG{'INT'} = 'quit';			# install signal handler for Ctrl-C
$SIG{'USR1'} = 'ReportWaiting';		# install USR1 handler
# getting this signal usually means that ssh is asking a question
#$SIG{'TTIN'} = 'IGNORE';		# stop waiting for input
# in case we miss a child finishing during the forking time,
#  we want to catch it, rather than have it get blocked and forgotten.
$SIG{'CHLD'} = 'gsh_catch' if ($signals);

# Figure out hostnames for self
my $self_host = `uname -n`;
chomp($self_host);
my $self_host_short = $self_host;
$self_host_short=~s/\..*$//;

# Our (real, not effective) user name
my $username = (getpwuid($<))[0];

my @ssh_args = ();
my $remote_user = $opt_force_user || $opt_user;

if ($opt_copy_to) {
	# We also need to validate that entries can be copied
	foreach my $file (@cmd) {
		die "$me: '$file' does not exist\n" unless -e $file;
		die "$me: '$file' is a device\n" if (-b _ || -c _);
		die "$me: '$file' is a pipe\n" if -p _;
		die "$me: '$file' is a socket\n" if -S _;
		die "$me: '$file' is unreadable\n" unless -r _;
	}
} else {
	# SSH arguments
	push(@ssh_args, "-n") if $opt_open_stdin;
	push(@ssh_args, "-l", $remote_user) if $remote_user;
	push(@ssh_args, split(/\s/, $opt_extra_ssh)) if length $opt_extra_ssh;
}

if ($opt_tabulate) {
	# Compute max host name length
	$opt_tabulate = max map { length($_->host) } @BACKBONES;
}
#
# Progress counters
my ($contacted, $replying, $skipped, $completed, $errored) = (0, 0, 0, 0, 0);
my @errored;
my @skipped;
my $hosts = @BACKBONES;		# Number of selected hosts
my $running_on_tty = -t STDIN && -t STDOUT;
our $last_length = 0;
our $last_state = "";

# Loop on each selected host
foreach my $ghost (@BACKBONES) {
	my $host = $ghost->host;
	my $port = $ghost->port;
	my $user = $ghost->user;

	tty_progress();

	# Check whether host is alive when --alive was given
	if ($opt_alive && !is_alive($host, $opt_alive)) {
		warn "$me: skipping non-responsive host '$host'\n";
		push(@skipped, $host);
		$skipped++;
		next;
	}

	# A -L USER supersedes any USER definition from ghosts
	undef $user if $opt_force_user;

	# clear this machine's output buffer
	$output{$host} = "";

	# make a column header for this machine if needed
	my $space = "";
	$space = " " x ($opt_tabulate - length($host)) if $opt_tabulate;
	$showlist{$host} = $opt_no_host_prefix ? "" : "$host$space: ";

#	push(@tried,$host);
	# do the fork
	my $pid = fork();			# fork
	if ($pid == 0) {

		open(STDOUT, ">$TMP/gsh.$$");	# open stdout to tmp file
		open(STDERR, ">&STDOUT");	# dup stderr to stdout
						# this results in rather
						# broken output sometimes
						# maybe have two files?
        # get rid of STDIN (but after the reopens, so a new FD isn't fd 1)
		close(STDIN) unless $opt_open_stdin;

		select(STDERR); $|=1;		# set outputs to unbuffered
		select(STDOUT); $|=1;

		my @list;

		if ($opt_run_locally) {
			# $var substitution done only when -r
			#
			# $host is the remote host
			# $port is the SSH port that will be used
			# $user is the SSH remote user used (from ghosts or -l / -L)

			my $sshport = $port || 22;
			my $sshuser = $username;
			$sshuser = $remote_user if $remote_user;
			$sshuser = $user if defined($user);

			@list = map {
				s/\$host/$host/g;
				s/\$port/$sshport/g;
				s/\$user/$sshuser/g;
				$_
			} @cmd;
		} else {
			@list = @cmd;
		}

		my $dir = '';
		if ($opt_copy_to) {
			$dir = $opt_copy_to;
			$dir .= "/" unless $dir =~ m|/$|;
		}

		if (
			$opt_run_locally || (
				!$opt_self_remote && (
					($host !~ /\./ && $host eq $self_host_short) ||
					$host eq $self_host
				)
			)
		) {
			if ($opt_copy_to) {
				unshift(@list, "cp");
				push(@list, $dir);
			}
			exec @list;		# exec the cmd locally
		} else {
			my @extra = ();
			my @run;
			if ($opt_copy_to) {
				push(@extra, "-P", $port) if defined $port;
				$user = $remote_user if ($remote_user && !defined($user));
				my $target = defined($user) ? "$user\@host" : $host;
				push(@list, "$target:$dir");
				@run = ("scp", "-rp", "-o", "BatchMode=yes", @extra, @list);
			} else {
				push(@extra, "-p", $port) if defined $port;
				push(@extra, defined($user) ? "$user@$host" : $host);
				@run = ("ssh", @ssh_args, "-o", "BatchMode=yes", @extra, @list);
			}
			exec @run;
		}

		# should never get to next line
		die "$me: exec of ssh to $host failed: $!!\n";
	}
	elsif (!$pid) {				# report failures
		# !$pid is true for 0 also...
		warn "$me: couldn't fork for '$host': $!\n";
	}
	else {
		print "#spawned $pid for $host\n" if $opt_debug;
		print STDERR "$host " if $showpid;
		$pidlist{$pid} = $host;		# record the child's pid
	}
}
close(STDIN);

my $waitfail = 0;
#$forked = join(' ',@tried);

# sometimes wait will return a -1.  I'm not sure what this is.  I've read
# too many different man pages on wait, and r3,r4, and aix all handle things
# differently.  My solution is to ignore -1's, and continue waiting.

# but since I'm not using r3 anymore, $signal is undef, and I use waitpid

my $cycles = 0;
my @left = keys %pidlist;
my $togo = $#left;
my $before = $togo;
while (defined($togo)) {
	# every 5 cycles (cycle == .5 seconds) we should EXPLICITLY
	# wait on a child.  Sometimes children don't get reaped
	# correctly by the SIGCHLD handler, so we need to wait
	# on them and call the handler directly.  This seemed to
	# fix all my problems with catching children.

	# the debugging output will show lists of what machine are still
	# being waited on, etc
	print STDERR "[$togo]\n" if ($opt_debug);

	if ($before != $togo) {
		$cycles = 0;
		$before = $togo;
	}
	else {
		$cycles++;
	}

	if ($cycles >= ($signals ? 5 : 0)) {
		if ($viewwaiting || $opt_debug) {
			$viewwaiting = 0;
			tty_progress_clear();
			print STDERR "Waiting on: ";
			foreach (keys %pidlist) {
				print STDERR "$pidlist{$_} ";
			}
			print STDERR "\n";
		}
		# if we catch something greater than 0, call SIGCHLD directly
		if ((my $pid = waitpid(-1, &WNOHANG)) > 0) {
			gsh_catch('', $pid, $?);
		}
	}

	show_output(!$opt_immediate);
	tty_progress();

	# wait for a half second
	select(undef,undef,undef,0.5) if ($opt_debug);
	#
	# see which processes are left
	@left = keys %pidlist;
	# update the "how many are left?" counters
	undef $togo;
	$togo = $#left if @left;

	$replying = $completed;
	foreach my $pid (@left) {
		$replying ++ if -s "$TMP/gsh.$pid";
	}
}

# handle any other output that hadn't been printed yet
show_output(0);
tty_progress_clear();

report_error("skipped %d host%s", \@skipped);
report_error("error reported for %d host%s", \@errored);

#print "skipped machines: $forked\n";
#@tried=split(/\s+/,$forked);
#
#foreach (@tried) {
#	print "No report: $_\n";
#}

exit($errored ? EXIT_ERRORED : EXIT_OK);



# subroutines

# Clear previous tty progress by overwriting a blank line.
sub tty_progress_clear {
	return unless $running_on_tty;
	print STDOUT "\r", " " x length($last_state), "\r";
	$last_state = "";
}

# Show progress if running on a tty
sub tty_progress {
	return unless $running_on_tty;
	my $msg = "[hosts=$hosts done=$completed reply=$replying";
	$msg .= " skip=$skipped" if $skipped;
	$msg .= " error=$errored" if $errored;
	$msg .= "]";
	return if $msg eq $last_state;
	$last_state = $msg;
	print STDOUT "\r";
	my $len = length $msg;
	$msg .= ' ' x ($last_length - $len) if $len < $last_length;
	$last_length = $len;
	print STDOUT $msg;
}

sub report_error {
	my ($fmt, $aref) = @_;
	return unless @$aref;
	my $cnt = @$aref;
	my $list = join(', ', @$aref);
	my $msg = "$me: " . sprintf($fmt, $cnt, $cnt > 1 ? "s" : "");
	if (length($msg) + length($list) < 77) {
		warn "$msg ($list)\n";
	} else {
		warn "$msg:\n";
		warn "$me: " . (' ' x 3) . $list . "\n";
	}
};

sub show_output {
	my ($waiting) = @_;
	# This loop checks to see if there is any output waiting to be
	# printed.  Since we're doing it alphabetically by machine name,
	# it will quit immediately if it comes across an "empty" output
	# in the alpha-sorted list of keys.
	# A lone "." means that a machine finished without any output.
	foreach my $key (sort keys %output) {
		if ($output{$key} ne "") {
			tty_progress_clear();
			print $output{$key} unless $output{$key} eq ".";
			delete $output{$key};
			$completed++;
		}
		elsif ($waiting) {
			last;
		}
	}
}

# Check whether given host is alive on the network
sub is_alive {
	my ($host, $secs) = @_;
	system "ping -w $secs -q -c 1 $host >/dev/null 2>&1";
	return 0 == $?;
}

sub quit {
	$| = 1;
	print "\r\n#caught SigInt...\n" if ($opt_debug);
	# clear handlers
	$SIG{'INT'} = '';
	$SIG{'QUIT'} = '';
	# for each child, kill the child, then unlink it's output file
	foreach my $pid (keys %pidlist) {
		print "#cleaning up pid: $pid\n" if ($opt_debug);
		my $host = $pidlist{$pid};
		warn "$me: sent INTR to stop command to host '$host'\n"
			if kill(2, $pid);
		grab_output($pid, 'interrupting', 0);
		print $output{$host} unless $output{$host} eq ".";
		unlink("$TMP/gsh.$pid");
	}
	# kill self, but not with signal to allow /tmp cleanup
	exit EXIT_QUIT;
}

# Grap output from finished child within $output{$host}
sub grab_output {
	my ($pid, $type, $status) = @_;
	# which machine finished?
	my $host = $pidlist{$pid};
	print "\n# $type $pid $host\n" if $opt_debug;
	$output{$host} .= banner($host) if $opt_banner;
	$output{$host} .= $showlist{$host} . join(' ', @cmd) . "\n"
		if $opt_show_command;
	if ($status != 0) {
		$output{$host} .= $showlist{$host} . "*** exit code was $status ***\n";
		$errored++;
		push(@errored, $host);
	}
	# make a unique filehandle name: handler needs to be reentrant
	my $READ = undef;		#time . "$pid";
	if (!open($READ, "<$TMP/gsh.$pid")) {
		$output{$host} .= $showlist{$host} . "error with output read: $!\n";
	}
	my $length = $opt_show_command + ($status != 0);
	local $_;
	while (<$READ>) {
		$output{$host} .= $showlist{$host} . $_;
		$length += length($_) unless $length;
	}
	# if there was no output, signal to the output printing loops
	if (0 == $length) {
		$output{$host} = "."
	} elsif ($type =~ /^interrupt/)  {
		$output{$host} .= "\n" if "\n" ne substr($output{$host}, -1);
		$output{$host} .= $showlist{$host} . "(interrupted by signal)\n";
	}
	close($READ);
}

# sig handler for when a child dies
sub gsh_catch {
	# first arg is signal caught, others only come if we force a call
	my ($undef, $forwarded, $status) = @_;
	my ($pid, $host, $type);

	if ($forwarded) {
		$pid = $forwarded;
		$type = "forwarded";
	}
	else {
		# get the pid of the dead child
		$pid = wait;
		$type = "caught";
	}
	# yell if wait is lying to us
	if ($pid < 0) {
		print "Missed a child??!  May have to Ctrl-C out.\n";
	}
	else {
		grab_output($pid, $type, $status);
		unlink("$TMP/gsh.$pid");	# clean up
#		$forked =~ s/$pidlist{$pid}//;
		delete $pidlist{$pid};		# remove from pending pid list
	}
}

sub ReportWaiting {
	$viewwaiting = 1;
	# on bad systems, you may need to do this
	#$SIG{'USR1'} = 'ReportWaiting';		# install USR1 handler
}

# Generate 80-column banner string with centered host name.
sub banner {
	my ($host) = @_;
	my $c = '=';
	my $prefix = $showlist{$host};
	my $len = 80 - 2 - length($host) - length($prefix);
	return $prefix .
		$c x int($len / 2 + 0.5) . " $host " . $c x int($len / 2) . "\n";
}

sub Version {
	print "$NAME version $VERSION\n";
	print <<'EOM';

Copyright 1998-2014 Kees Cook <kees@outflux.net>
Copyright 2021 Raphael Manfredi <Raphael_Manfredi@pobox.com>

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.
EOM
    exit EXIT_OK;
}

=head1 PREREQUISITES

C<POSIX>
C<File::Temp>

=head1 EXIT VALUES

=over 8

=item B<0>

Success

=item B<1>

Error in option parsing

=item B<2>

No command was specified

=item B<3>

An error occurred in one of the hosts

=item B<4>

The command was interrupted

=back

=head1 ENVIRONMENT VARIABLES

The following environment variable is used:

=over 8

=item B<GSH_HOSTS>

This variable, when set, is used to set the default path of the C<ghosts>
file.  It will be ignored when the B<-g> switch is used.

=back

=head1 BUGS

We bet.

=head1 FILES

/etc/ghosts

=head1 SEE ALSO

perl(1), ghosts(1), ssh(1).

=head1 AUTHORS

Kees Cook E<lt>kees@outflux.netE<gt>

Raphael Manfredi E<lt>Raphael_Manfredi@pobox.comE<gt>

L<http://www.outflux.net/|http://www.outflux.net/>

=head1 COPYRIGHT

Copyright (C) 1998-2014 Kees Cook <kees@outflux.net>

Copyright (C) 2021 Raphael Manfredi <Raphael_Manfredi@pobox.com>

Supposedly based on original code distributed with Perl Distribution.

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

=cut

# vi: set ts=4 sw=4:
