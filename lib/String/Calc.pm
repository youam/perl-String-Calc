package String::Calc;

use 5.006;
use strict;
use warnings FATAL => 'all';

use Number::Format;
use YAML::Syck;
use Math::Round;
use utf8;

#use Smart::Comments;

use strict;
use Carp;
use overload
  '+'    => '_add',
  '-'    => '_sub',
  '*'    => '_mul',
  '/'    => '_div',

  '=='   => '_ieq',
  '<=>'  => '_icmp',
  '>'    => '_igt',
  '>='   => '_igte',
  '<'    => '_ilt',
  '<='   => '_ilte',

  'eq'   => '_seq',
  'ne'   => '_sne',

  '""'   => 'as_string';

use warnings::register;

=head1 NAME

String::Calc - The great new String::Calc!

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';


=head1 SYNOPSIS

Quick summary of what the module does.

Perhaps a little code snippet.

    use String::Calc;

    my $foo = String::Calc->new();
    ...

=head1 EXPORT

A list of functions that can be exported.  You can delete this section
if you don't export anything, such as for a purely object-oriented module.

=head1 SUBROUTINES/METHODS

=head2 function1

=cut

=head2 function2

=cut

=head1 AUTHOR

Uli Martens, C<< <uli at youam.net> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-string-calc at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=String-Calc>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.




=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc String::Calc


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=String-Calc>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/String-Calc>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/String-Calc>

=item * Search CPAN

L<http://search.cpan.org/dist/String-Calc/>

=back


=head1 ACKNOWLEDGEMENTS


=head1 LICENSE AND COPYRIGHT

Copyright 2016 Uli Martens.

This program is free software; you can redistribute it and/or modify it
under the terms of the the Artistic License (2.0). You may obtain a
copy of the full license at:

L<http://www.perlfoundation.org/artistic_license_2_0>

Any use, modification, and distribution of the Standard or Modified
Versions is governed by this Artistic License. By using, modifying or
distributing the Package, you accept this license. Do not use, modify,
or distribute the Package, if you do not accept this license.

If your Modified Version has been derived from a Modified Version made
by someone other than you, you are nevertheless required to ensure that
your Modified Version complies with the requirements of this license.

This license does not grant you the right to use any trademark, service
mark, tradename, or logo of the Copyright Holder.

This license includes the non-exclusive, worldwide, free-of-charge
patent license to make, have made, use, offer to sell, sell, import and
otherwise transfer the Package with respect to any patent claims
licensable by the Copyright Holder that are necessarily infringed by the
Package. If you institute patent litigation (including a cross-claim or
counterclaim) against any party alleging that the Package constitutes
direct or contributory patent infringement, then this Artistic License
to you shall terminate on the date that such litigation is filed.

Disclaimer of Warranty: THE PACKAGE IS PROVIDED BY THE COPYRIGHT HOLDER
AND CONTRIBUTORS "AS IS' AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE, OR NON-INFRINGEMENT ARE DISCLAIMED TO THE EXTENT PERMITTED BY
YOUR LOCAL LAW. UNLESS REQUIRED BY LAW, NO COPYRIGHT HOLDER OR
CONTRIBUTOR WILL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, OR
CONSEQUENTIAL DAMAGES ARISING IN ANY WAY OUT OF THE USE OF THE PACKAGE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


=cut

sub as_string {
    my $self = shift;

    ### as_string : $self
    if ( $self->{presentation} eq "-xAPPLES" ) {
        my $str =  $self->value;
        $str .= $self->{unit};
        return $str;
    }
    if ( $self->{presentation} eq "-xxxxx,yyAPPLES" ) {
        my $f = Number::Format->new(
            THOUSANDS_SEP => "",
            DECIMAL_POINT => ",",
            MON_THOUSANDS_SEP => "",
            MON_DECIMAL_POINT => ",",
            DECIMAL_DIGITS => 2,
            DECIMAL_FILL => 1,
        );

        my $str = $f->format_number( $self->value );
        $str .= $self->{unit};
        return $str;
    }
    if ( $self->{presentation} eq "-xx.xxx.xxx,yyAPPLES" ) {
        my $f = Number::Format->new(
            THOUSANDS_SEP => ".",
            DECIMAL_POINT => ",",
            MON_THOUSANDS_SEP => ".",
            MON_DECIMAL_POINT => ".",
            DECIMAL_DIGITS => 2,
            DECIMAL_FILL => 1,
        );

        my $str = $f->format_number( $self->value );
        $str .= $self->{unit};
        return $str;
    }
    if ( $self->{presentation} eq "-x.yAPPLES" ) {
        return $self->value."".$self->{unit};
    }
    if ( $self->{presentation} eq "NULL" ) {
        ...;
        return $self->value; # either empty or undef
    }
    if ( $self->{presentation} eq "0" ) {
        return "0";
    }
    confess "don't know how to stringify format $self->{presentation}";
}

sub _create {
    my $class = shift;
    confess "broken args" unless scalar @_ eq 4;

    my $self = {};

    $self->{numerator} = shift;
    confess "numberator must be integer!"
        if defined $self->{numberator}
           and int($self->{numberator}) ne $self->{numberator};
    $self->{denominator} = shift;
    confess "denominator must be integer!"
        if defined $self->{denominator}
           and int($self->{denominator}) ne $self->{denominator};
    $self->{unit} = shift;
    $self->{presentation} = shift;

    return bless( $self, $class );
}

sub _clone {
    my $i = shift;
    ### clone an instance, so that the clone can be changed/updated
    my %i = %{$i};
    my %n = %i;
    my $n = \%n;
    return bless $n, ref $i;
}

sub value {
    my $self = shift;
    confess "oi! denominator is zero" if $self->{denominator} eq 0;
    # XXX hum. that's NOT really beautiful
    return $self->{numerator} / $self->{denominator};
}

my @_format_upgrade_table = (
    { l => "-xAPPLES", r => "-x.yAPPLES" },
    { l => "-xAPPLES", r => "-xx.xxx.xxx,yyAPPLES" },
    { l => "-x.yAPPLES", r => "-xx.xxx.xxx,yyAPPLES" },
    { l => "-xxxxx,yyAPPLES", r => "-xx.xxx.xxx,yyAPPLES" },
);
sub _format_upgrade {
    my $i = shift;
    my $j = shift;

    for my $u ( @_format_upgrade_table ) {
        return ( $i, $j ) if $i->{presentation} eq $j->{presentation};

        if ( $i->{presentation} eq $u->{l} and $j->{presentation} eq $u->{r} ) {
            ### upgrade presentation of $i
            $i = _clone( $i );
            $i->{presentation} = $j->{presentation};
        }
        if ( $i->{presentation} eq $u->{r} and $j->{presentation} eq $u->{l} ) {
            ### upgrade presentation of $j
            $j = _clone( $j );
            $j->{presentation} = $i->{presentation};
        }
    }
    return ( $i, $j );
}

sub _add {
    my $i = shift;
    my $j = shift;
    my $swapped = shift; # ignored for _add

    return $i if not defined $j;
    return $j if not defined $i;

    if ( ref $j eq "" ) {
        ### $j is a scalar, upgrade it to __PACKAGE__
        $j = String::Calc->new( $j );
        if ( not defined $j ) {
            ...
        }
    }
    if ( ref $i ne ref $j ) {
        my $ri = ref $i;
        my $rj = ref $j;
        confess "could not figure out what to do with an $ri and an $rj which are " .( $swapped ? "" : "not" )." swapped";
    }

    return $j if $i->{presentation} eq "";
    return $i if $j->{presentation} eq "";
    return $j if $i->value eq 0;
    return $i if $j->value eq 0;
    die unless defined $i->{unit}; #XXX do we need those?
    die unless defined $j->{unit}; #XXX do we need those?
    die "don't know what to do with: ". Dump $j unless defined $j->{unit};

    ( $i, $j ) = _format_upgrade( $i, $j );
    if ( $i->{presentation} eq $j->{presentation} and $i->{unit} eq " ".$j->{unit} ) {
        $j = _clone( $j );
        $j->{unit} = " " . $j->{unit};
    }
    if ( $i->{presentation} eq $j->{presentation} and " ".$i->{unit} eq $j->{unit} ) {
        $i = _clone( $i );
        $i->{unit} = " " . $i->{unit};
    }
    if ( $i->{presentation} eq $j->{presentation} and $i->{unit} eq $j->{unit} ) {
        confess if $i->{denominator} != $j->{denominator};
        my $n = _clone( $i );
        $n->{numerator} += $j->{numerator};

        return $n;
    }
    if ( (caller)[0] eq "Math::Round" ) { # and ref $j eq "" ) {
        confess( "rounding of fixed-precision calculation?!" );
        # Math::Round doesn't call us with correct units and stuff, just add it
        my $self = _clone( $i );
        # FIXME verify that numerator is still integer
        $self->{numerator} += $j / $self->{denominator};
        return $self;
    }
    confess ( __PACKAGE__ . "->_add got $i->{presentation}/$i->{unit} and $j->{presentation}/$j->{unit}");
}

sub _sub {
    my $i = shift;
    my $j = shift;
    my $swapped = shift;

    return $i if not defined $j;
    return $j if not defined $i;

    if ( ref $j eq "" ) {
        ### $j is a scalar, upgrade it to __PACKAGE__
        $j = String::Calc->new( $j );
        if ( not defined $j ) {
            ...
        }
    }
    if ( ref $i eq ref $j ) {
        ### got another __PACKAGE __
        if ( not $swapped ) {
            return ($j * -1) if $i->{presentation} eq "NULL";
            return ($i     ) if $j->{presentation} eq "NULL";
            return ($j * -1) if $i->value eq 0;
            return ($i     ) if $j->value eq 0;
            return ($i     ) if $j->{presentation} eq "ZERO";
            return ($j * -1) if $i->{presentation} eq "ZERO";
        } else {
            return ($j     ) if $i->{presentation} eq "NULL";
            return ($i * -1) if $j->{presentation} eq "NULL";
            return ($j     ) if $i->value eq 0;
            return ($i * -1) if $j->value eq 0;
            return ($i * -1) if $j->{presentation} eq "ZERO";
            return ($j     ) if $i->{presentation} eq "ZERO";
        }
        die unless defined $i->{unit};
        die "don't know what to do with: ". Dump $j unless defined $j->{unit};

        ( $i, $j ) = _format_upgrade( $i, $j );
        if ( $i->{presentation} eq $j->{presentation} and $i->{unit} eq " ".$j->{unit} ) {
            $j = _clone( $j );
            $j->{unit} = " " . $j->{unit};
        }
        if ( $i->{presentation} eq $j->{presentation} and " ".$i->{unit} eq $j->{unit} ) {
            $i = _clone( $i );
            $i->{unit} = " " . $i->{unit};
        }

        if ( $i->{presentation} eq $j->{presentation} and $i->{unit} eq $j->{unit} ) {
            confess if $i->{denominator} != $j->{denominator};
            my $n = _clone( $i );
            if ( not $swapped ) {
                $n->{numerator} -= $j->{numerator};
            } else {
                $n->{numerator} = $j->{numerator} - $n->{numerator};
            }
            ## # i : $i
            ## # j : $j
            # ## prepending blank to unit for j not really

            return $n;
        }
        if ( (caller)[0] eq "Math::Round" ) { # and ref $j eq "" ) {
            # Math::Round doesn't call us with correct units and stuff, just add it
            my $self = _clone( $i );
            if ( not $swapped ) {
                $self->{value} -= $j;
            } else {
                $self->{value} = $j - $self->{value};
            }
            return $self;
        }
        confess ( __PACKAGE__ . "->_sub got $i->{presentation}/$i->{unit} and $j->{presentation}/$j->{unit}");
    }
    ### something else
    my $ri = ref $i;
    my $rj = ref $j;
    die "could not figure out what to do with an $ri and an $rj which are $swapped? swapped";
}

sub _mul {
    my $i = shift;
    my $j = shift;
    my $swapped = shift; # ignored

    if ( ref $j eq "" and $j =~ m/^-?[0-9]+(.[0-9]+)?$/ ) {
        # $j is a plain perl scalar
        confess "don't want to multiply with a fraction ($j)" if int($j) != $j;
        my $n = _clone( $i );
        $n->{numerator} *= $j;
        return $n;
    }

    confess ( __PACKAGE__."->_mul doesn't know how to (" . (Dump $i) .", " . (Dump $j) ." )" );
}
sub mul {
    my $i = shift;
    my $j = shift;
    my $options = shift;

    #### in String_Calc_mul
    #### $i
    #### $j
    #### $options

    my $r = ref $j;
    if ( ref $j eq "" and $j =~ m/^-?[0-9]+(.[0-9]+)?$/ ) {
        # $j is a plain perl scalar
        die unless defined $options->{round} and $options->{round} eq "nearest";
        my $self = _clone( $i );
        #### $self
        $self->{numerator} *= $j;
        #### num : $self->{numerator}
        $self->{numerator} = nearest( 1, $self->{numerator} );
        #### num : $self->{numerator}
        #### $self
        return $self;
    }

    confess ( __PACKAGE__."->_mul doesn't know how to (" . (Dump $i) .", " . (Dump $j) ." )" );
}

sub _div {
    my $i = shift;
    my $j = shift;
    my $swapped = shift;

    my $class = ref $i;

    ### got another __PACKAGE __
    my $r = ref $j;
    ### ref j is : $r
    die if $swapped;
    if ( ref $i eq ref $j ) {
        if ( $i->{unit} eq $j->{unit} ) {
            # FIXME unit/unit -> no_unit
            confess if $i->{denominator} != $j->{denominator};
            my $val = $swapped
                ? $j->{denominator} / $i->{denominator}
                : $i->{denominator} / $j->{denominator}
                ;
            return $class->_create( $val, $i->{denominator}, $i->{unit}, $i->{presentation} );
        }
        ...;
    }
    if ( ref $j eq "" and $j =~ m/^-?[0-9]+(.[0-9]+)?$/ ) {
        ### foo!
        my $n = _clone( $i );
        $n->{numerator} /= $j;
        return $n;
    }

    confess ( __PACKAGE__."->_div doesn't know how to (" . Dump $i .", " . Dump $j ." )" );
    ...
}

sub __compare {
    my $i = shift;
    my $j = shift;
    my $swapped = shift;
    my $comp = shift;

    confess "can't compare undef" unless defined $i;
    confess "can't compare undef" unless defined $j;



    my $ret;
    if( ref $j eq "" ) {
        # XXX should we always try to compare perlified values or should '0' be
        #     a special case?
        if ( not $swapped ) {
            $ret = &{$comp}($i->value, $j);
        } else {
            $ret = &{$comp}($j, $i->value);
        }
    }
    if ( ref $i eq ref $j ) {
        if ( $i->{presentation} ne "0" and $j->{presentation} ne "0" ) {
            confess "units differ, got '".$i->{unit}."' and '". $j->{unit}. "'" if $i->{unit} ne $j->{unit};
        }
        if ( not $swapped ) {
            $ret = &{$comp}($i->value, $j);
        } else {
            $ret = &{$comp}($j, $i->value);
        }
    }
    die unless defined $ret;
    return $ret;
}

# XXX the following COMPLETELLY IGNORES units and formatting.
# 1.00 APPLE == 1BANANA
sub _ieq  { return __compare( @_, sub { my ( $a, $b ) = @_; return $a ==  $b } ); }
sub _icmp { return __compare( @_, sub { my ( $a, $b ) = @_; return $a <=> $b } ); }
sub _igt  { return __compare( @_, sub { my ( $a, $b ) = @_; return $a >   $b } ); }
sub _igte { return __compare( @_, sub { my ( $a, $b ) = @_; return $a >=  $b } ); }
sub _ilt  { return __compare( @_, sub { my ( $a, $b ) = @_; return $a <   $b } ); }
sub _ilte { return __compare( @_, sub { my ( $a, $b ) = @_; return $a <=  $b } ); }
sub _sne  { return __compare( @_, sub { my ( $a, $b ) = @_; return $a ne  $b } ); }
sub _seq  { return __compare( @_, sub { my ( $a, $b ) = @_; return $a eq  $b } ); }


sub _inval {
    my ($first);
    $first = shift;
    Carp::croak( "Invalid "
          . ( ref($first) || $first )
          . " constructor args: ('"
          . join( "', '", @_ )
          . "')" );
}

our $suffix = {
    '€' => {
        can_have_space => 1,
    },
    'EUR' => {
        alias => '€',
        can_have_space => 1,
    },
    'RATE' => {
        alias => 'RATE',
        can_have_space => 1,
    },
};

sub _new {
    my ( $that, @input ) = @_;

    my $class = ref($that) || $that;

    if ( scalar @input == 2 ) {
        # got called as __PACKAGE__->new( $value, $unit );
        ...
    }

    if ( scalar @input == 1 ) {
        my $x = $input[0];

        # got called with single argument of type __PACKAGE__
        return $x if UNIVERSAL::isa( $x, __PACKAGE__ );

        # __PACKAGE__->new( undef ) => undef
        return if not defined $x;

        # __PACKAGE__->new( "" )
        if ( $x eq "" ) {
            # XXX Are we sure what should happen here?
            # XXX former comment said "either empty or undef. do we want to
            #      differentiate between those?
            return $class->_create( $x, undef, "", "" );
        }
        if ( $x eq "0" ) {
            return $class->_create( 0, 1, "0", "0" );
        }

        my $unit;
        if ( $x =~ m/^[0-9]+(.[0-9]+)?$/ or $x =~ m/^[0-9]+e-?[0-9]+$/ ) {
            # this is a unit-less perl scalar thingy. add dummy unit;
            ### ADD PERLFLOAT TO : $x
            #$x .= "PERLFLOAT";
            $unit = "";
        } else {
            my @suf_matches;
            for my $suf ( keys %{$suffix} ) {
                if ( $x =~ m/$suf$/ ) {
                    push @suf_matches, $suf;
                }
            }
            if ( scalar @suf_matches > 1 ) {
                # handle more than one possible suffix
                die "i've got ".(scalar @suf_matches)." suffix matches for: ",join(",",@suf_matches);
                ...
            }
            if ( scalar @suf_matches == 0 ) {
                confess "got no suffix for $x";
            }
            if ( scalar @suf_matches == 1 ) {
                my $suf = $suf_matches[0];
                # FIXME implement ->can_have_space
                if ( not defined $suffix->{$suf}->{can_have_space} or $suffix->{$suf}->{can_have_space} ) {
                    $x =~ s/( ?$suf)//;
                    $unit = $1;
                } else {
                    $x =~ s/($suf)$//;
                    $unit = $1;
                }
                die "unit undefined for $x" unless defined $unit;
            }
        }

        if ( $x =~ /^(?<negated>-?)(?<integers>[0-9]+)$/ ){
            return $class->_create( $x, 1, $unit, '-xAPPLES' );
        }
        elsif ( $x =~ /^(?<negated>-?)(?<integers>[0-9]{1,3}(\.[0-9]{3})*),(?<decimals>[0-9]{2})$/ ) {
            # -xx.xxx.xxx,yy EUR
            my $val = $x;
            $val =~ y/\.//d;
            $val =~ y/,/./;
            $val *= 100;
            return $class->_create( $val, 100, $unit, '-xx.xxx.xxx,yyAPPLES' );
        }
        elsif ( $x =~ /^(?<negated>-?)(?<integers>[0-9]+),(?<decimals>[0-9]{2})$/ ) {
            # -xxxxx,yy EUR
            my $val = $x;
            $val =~ y/,/./;
            $val *= 100;
            return $class->_create( $val, 100, $unit, '-xxxxx,yyAPPLES' );
        }
        elsif ( $x =~ /^(?<negated>-?)(?<integers>[0-9]+).(?<decimals>[0-9]+)$/ ) {
            # -x.y EUR
            # Ugh. that's an ugly heuristic
            my $val = $+{integers};
            my $dlen = length $+{decimals};
            if ( $dlen > 5 ) {
                die "does $x really have more than $dlen significant decimals?";
            }
            $val *= 10 ** $dlen;
            $val += $+{decimals};
            $val *= -1 if $+{negated} eq "-";

            return $class->_create( $val, 10 ** $dlen, $unit, '-x.yAPPLES' );
        }
        else {
            if ( defined $unit ) {
                confess "unparsed value $x with unit $unit";
                ...
            }
            die "unparsed amount $x";
            ...;
            return (undef);
            }
            #    elsif ($x =~ /^(\d\d\d\d)-(\d\d)-(\d\d)$/
            #|| $x =~ /^(\d\d\d\d)(\d\d)(\d\d)$/ ) {
            #@input = ( $1, $2, $3 );
            #}
            # else {
            #return (undef);
            # }
    }    # we fall through here...

    # to get here, we had one arg which was split,
    # or 3 in the first place
    if ( scalar @input == 2 ) {
        # FIXME parse number from $input[0]
        my $self = { value => $input[0], unit => $input[1] };
        bless( $self, $class );
        ### self is : $self
        return $self;
    }

    $class->_inval(@input);
}


sub new {
    my ( $class, $amount );

    $amount = &_new;
    if ( ( not defined $amount ) && scalar(@_) == 1 ) {
        Carp::croak( "'" . shift() . "' was not parsed" );
    }
    return $amount;
}


1;
# vim: ts=4 sw=4 expandtab
