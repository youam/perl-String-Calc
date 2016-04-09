#!perl -T
use 5.006;
use strict;
use warnings FATAL => 'all';
use Test::More;

plan tests => 1;

BEGIN {
    use_ok( 'String::Calc' ) || print "Bail out!\n";
}

diag( "Testing String::Calc $String::Calc::VERSION, Perl $], $^X" );
