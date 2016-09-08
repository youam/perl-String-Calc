#!perl -T
use 5.006;
use strict;
use warnings FATAL => 'all';
use Test::More;

use String::Calc;

my $amount;

$amount = String::Calc->new( '12.345.678,90 EUR' ); is( $amount . '', '12.345.678,90 EUR', 'str(a X) => a X'     ); is( $amount->value, 12345678.90, 'val(a X) => a' );
$amount = String::Calc->new( '12.345.678,90EUR'  ); is( $amount . '', '12.345.678,90EUR',  'str(aX) => aX'       ); is( $amount->value, 12345678.90, 'val(aX) => a' );
$amount = String::Calc->new( undef               ); is( $amount,      undef,               '__PACKAGE__->new(undef) => undef' ); # FIXME this is probably broken. check if is() works that way
$amount = String::Calc->new( 0                   ); is( $amount . '', '0',                 'str(0) => 0'         ); is( $amount->value, 0,           'val(0) => 0' );
$amount = String::Calc->new( 1                   ); is( $amount . '', '1',                 'str(1) => 1'         ); is( $amount->value, 1,           'val(1) => 1' );
$amount = String::Calc->new( 1.2                 ); is( $amount . '', '1.2',               'str(1.2) => 1.2'     ); is( $amount->value, 1.2,         'val(1.2) => 1.2' );

done_testing();
