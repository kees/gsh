use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'      => 'gsh',
    'VERSION_FROM' => 'gsh', # finds $VERSION
    'ABSTRACT' => "Distributed Global Hosts tool",
    dist         => { COMPRESS => 'gzip -9f', SUFFIX => '.tar.gz', },
    ($] < 5.005 ? () : (
        AUTHOR  => 'Kees Cook <kees@outflux.net>'
    )),
    'EXE_FILES' => [qw(gsh ghosts)],
);

