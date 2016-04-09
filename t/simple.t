#!perl -T
use 5.006;
use strict;
use warnings FATAL => 'all';
use Test::More;

plan tests => 2;


use String::Calc;

my $amount = String::Calc->new( '12.345.678,90 EUR' );

is( ($amount + $amount)."",  '24.691.357,80 EUR', 'a+a => 2a' );
is( ($amount * 10)."" ,     '123.456.789,00 EUR', 'a*10 => 10a' );

done_testing();



