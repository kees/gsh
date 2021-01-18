package SystemManagement::Ghosts;

use warnings;
use strict;

my $GHOSTS_PATH = "/etc/ghosts";
my @GHOSTS;		# all the lines of the ghosts file
my %TAGS;		# macros and tags
my %MACROS;		# macro definitions
my %HOSTS;		# known hosts

my $me = $0;
$me =~ s|.*/(.*)|$1|;

# Create a new ghost
# The given ghost specification is [user@]host[:port]
# User and port will remain 'undef' if not specified.
# It is up to the client code to do the right thing.
sub make {
	my $self = bless {}, shift;
	my ($ghost) = @_;
	$self->{user} = $1 if $ghost =~ s/^(\w+)@//;
	$self->{port} = $1 if $ghost =~ s/:(\d+)$//;
	$self->{host} = $ghost;
	return $self;
}

sub user { $_[0]->{user} }
sub port { $_[0]->{port} }
sub host { $_[0]->{host} }

# loads the ghosts file into @GHOSTS
sub Load {
	my ($file) = @_;
	$file = $GHOSTS_PATH if (!defined($file) || $file eq "");
	$GHOSTS_PATH = $file;		# The file we're loading becomes the default
	open(GHOSTS_FILE, "<${file}") ||
		die "$me: cannot open host file \"${file}\": $!\n";
	local $_;
	while (<GHOSTS_FILE>) {
		# kill blank lines
		s/^\s+//;
		# kill commented or blank lines
		next if /^#/ || /^\s*$/;
		push(@GHOSTS, $_);
	}
	close(GHOSTS_FILE);
}

sub ParseGhosts {
	my ($depth, @what) = @_;

	if ($depth > 10) {
		warn "ParseGhosts: recurrsive macro?  Depth == 10 at '" .
			join(',',@what) . "'\n";
		return ("");
	}

	# if no argument, match all machine names
	@what = ("all") unless @what;

	# make the starting matching string, ":" separated
	my $one_of_these = ":" . join("+", @what) . ":";   # prepare to expand "macros"
	$one_of_these =~ s/\s+//g;
	$one_of_these =~ s/\+/:/g;           # we hope to end up with list of
	$one_of_these =~ s/\^/:^/g;          #  colon separated attributes
	$one_of_these =~ s/:+/:/g;

#	warn "ParseGhosts[$depth]: expanding '$one_of_these'\n";
	my @repl;
	Load() if ($#GHOSTS < 0);
	my %warned;
	foreach my $line (@GHOSTS) {            # for each line of ghosts
		if ($line =~ /^(\w+)\s*=\s*(.+)/) { # a macro line?
			my $name = $1;
			my $repl = $2;
			if (0 == $depth) {
				warn "$me: '$name' macro redefined\n"
					if (exists($MACROS{$name}) && !$warned{"M:$name"}++);
				$TAGS{$name}++;
				$MACROS{$name} = $repl;
			}
			$repl =~ s/\s*\+\s*/:/g;	# make an addition
			$repl =~ s/\s*\^\s*/:^/g;	# subtract
			# do expansion in "wanted" list
			if ($one_of_these =~ /:$name:/) {
				my @query = map { $_->host } ParseGhosts($depth+1, $repl);
				$repl = ":" . join(":", @query) . ":";
#				warn "'$one_of_these' matched '$name' as '$repl'\n";
				$one_of_these =~ s/:$name:/:$repl:/;
			}

			# do expansion in "unwanted" list
			if ($one_of_these =~ /:-$name:/) {
				my @query = map { $_->host } ParseGhosts($depth+1, $repl);
				$repl = ":" . join(":", @query) . ":";
				$repl =~ s/:/:^/g;
#				warn "'$one_of_these' matched '-$name' as '$repl'\n";
				$one_of_these =~ s/:\^$name:/:$repl:/;
			}
		}
		else {
			# we have a normal line

			# list of attributes to match against
			# which we put into an array normalized to lower case
			my @attr = split(' ', lc($line));

			# The first attribute can be more than a host name
			my $ghost = SystemManagement::Ghosts->make($attr[0]);
			my $host = $attr[0] = $ghost->host;

			if (0 == $depth) {
				# Ensure host is not already a known tag
				if (exists($TAGS{$host}) && !$warned{"L:$host"}++) {
					warn sprintf("$me: host '$host' is also a listed %s\n",
						exists($MACROS{$host}) ? "macro" : "tag");
				}
				# Ensure host is not redefined
				warn "$me: host '$host' redefined\n"
					if (exists($HOSTS{$host}) && !$warned{"R:$host"}++);
				$HOSTS{$host}++;

				# Record all the tags
				my %seen;
				for (my $i = 1; $i < @attr; $i++) {
					my $tag = $attr[$i];
					warn "$me: tag '$tag' for host '$host' is also a macro\n"
						if (exists($MACROS{$tag}) && !$warned{"T:$host:$tag"}++);
					warn "$me: multiple tag '$tag' for host '$host'\n"
						if ($seen{$tag}++ && !$warned{"D:$host:$tag"}++);
					$TAGS{$tag}++;
				}
			}

			my $wanted = 0;
			foreach my $attr (@attr) { # iterate over attribute array
				if (index(lc($one_of_these),":$attr:") >= 0) {
					$wanted++;
				}
				if (index(lc($one_of_these),":^$attr:") >= 0) {
					$wanted = 0;
					last;
				}
			}
			push(@repl, $ghost) if $wanted > 0;
		}
	}
	return @repl;
}

# sets the BACKBONES variable depending on argument
sub Expanded {
	my (@type) = @_;
	return map { $_->host } ParseGhosts(0, @type);
}

# Get the objects matching @type
sub Objects {
	my (@type) = @_;
	return ParseGhosts(0, @type);
}

# Returns the known macros
sub Macros {
	my @list;
	foreach my $key (sort keys %MACROS) {
		my $value = $MACROS{$key};
		# Beautify by making operators stand out
		$value =~ s/(\S)([\+^])/$1 $2/g;
		$value =~ s/([\+^])(\S)/$1 $2/g;
		push(@list, "$key = $value");
	}
	return @list;
}

# Returns the known tags that can be used to select hosts
sub Tags {
	return sort keys %TAGS;
}

# Returns the known hosts
sub Hosts {
	return sort keys %HOSTS;
}

# Returns list of array reference, each entry containing:
# [ tag, "macro" or "tag", count]
sub TagArray {
	my @list;
	foreach my $tag (sort keys %TAGS) {
		my $type = "tag";
		my $count = $TAGS{$tag};
		if (exists $MACROS{$tag}) {
			$type = "macro";
			# Use depth=1 to avoid re-counting and subsequent warnings
			my @hosts = ParseGhosts(1, $tag);
			$count = @hosts;
		}
		push(@list, [$tag, $type, $count]);
	}
	return @list;
}

1;

# vi: set ts=4 sw=4:
