use warnings;
use strict;

use Test::More;
use File::Path qw(make_path);
use File::Temp qw(tempdir);
use File::Basename qw(dirname);

use SystemManagement::Ghosts qw(UserConfig);

# utility function to easily create temporary files
sub touch {
   my $file = shift;
   make_path(dirname($file));
   open my $fh, '>', $file or die 'can\'t create temporary file';
}

# change $HOME to a temp dir
$ENV{HOME} = tempdir( CLEANUP => 1 );
my $HOME = $ENV{HOME};

is	'', UserConfig(), 'no user-config found';

touch "$HOME/etc/ghosts";
is	"$HOME/etc/ghosts", UserConfig(), 'must find $HOME/etc/ghosts file';

touch "$HOME/.config/ghosts";
is	"$HOME/.config/ghosts", UserConfig(), 'must find $HOME/.config/ghosts file';

touch "$HOME/.ghosts";
is	"$HOME/.ghosts", UserConfig(), 'must find $HOME/.ghosts file';

done_testing();
