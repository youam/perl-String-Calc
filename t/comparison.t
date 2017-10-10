#!perl -T
use 5.006;
use strict;
use warnings FATAL => 'all';
use utf8;
use Test::More;

plan tests => 1;


use String::Calc;

my $amount = String::Calc->new( '12.345.678,90€' );

cmp_ok( $amount, '<', '98.765.432,10€', '12.345.678.90€ < 98.765.432,10€' );

done_testing();
