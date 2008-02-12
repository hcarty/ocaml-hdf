#!/usr/bin/perl

use strict;
use warnings;

use constant MAX_STRING_LENGTH => 1024;
use constant MAX_VAR_DIMS => 32;

my $input_file = $ARGV[0];
my $output_file = $ARGV[0] . ".inc";

# Attributes for functions and their arguments.
my %manual_function_attributes = (
    SDgetinfo => {
        parameter_attributes => {
            dimsizes => ["size_is(" . MAX_VAR_DIMS . ")"]
        }
    },
    SDcreate => {
        parameter_attributes => {
            dimsizes => ["size_is(rank)"]
        }
    },
);
# Functions which simply return a 1 or 0 for success vs failure.
my %returns_error_code = (
    Hclose => 1,

    Vend => 1,
    Vfinish => 1,
    Vinitialize => 1,
    Vstart => 1,

    VSdetach => 1,
    VSfdefine => 1,
    VSgetclass => 1,
    VSinquire => 1,
    VSsetclass => 1,
    VSsetfields => 1,
    VSsetinterlace => 1,
    VSsetname => 1,

    SDdiminfo => 1,
    SDend => 1,
    SDendaccess => 1,
    SDfileinfo => 1,
    SDgetinfo => 1,
);

=head2 read_file
Read in the file contents, return an array containing each of the lines.
=cut
sub read_file {
    my $file = $input_file;
    open my $fin, $file or die "Unable to open $file for reading: $!";
    my @lines = <$fin>;
    close $fin;
    return @lines;
}

=head2 clean_up
Some initial code cleanup.
This:
 * Removes comments
 * Removes preprocessor directives (though there shouldn't be any left)
 * Replaces strings of whitespace longer than 1 character with a single space
 * Puts a newline after each ;
=cut
sub clean_up {
    my $code = shift;

    # Comments
    # Taken from perlfaq6
    $code =~ s#/\*[^*]*\*+([^/*][^*]*\*+)*/|//[^\n]*|("(\\.|[^"\\])*"|'(\\.|[^'\\])*'|.[^/"'\\]*)#defined $2 ? $2 : ""#gse;
    # Preprocessor stuff
    $code =~ s/^\#.*(\\\n.*)*//mgo;
    # Shrink the whitespace
    $code =~ s/\s+/ /g;
    # Add newlines back in to separate function definitions
    $code =~ s/; /;\n/g;
    return $code;
}

=head2 make_attribute_string
Description:
Return a string containing the properly formatted text for the given array
of attributes.

Arguments:
Array of attributes to use (NOT arrayref).

Returns:
String of attributes, include surrounding square brackets.
=cut
sub make_attribute_string {
    return "[" . (join ', ', @_) . "]";
}

=head2 minimize_whitespace
Description:
Strip leading, trailing whitespace from each argument.  Also compress any
internal whitespace to a single space character.

Arguments:
Array of string to modify (they are modified in place!)

Returns:
Nothing important.
=cut
sub minimize_whitespace {
    for (@_) {
        # Strip leading
        s/^\s+//;
        # Strip trailing
        s/\s+$//;
        # Collapse multiple whitespace characters to one space
        s/\s+/ /;
    }
}

=head2 function_attributes
Description:
Given a function name and return type, determine its attributes.

Arguments:
1. Function return type
2. Function name

Returns:
=cut
sub function_attributes {
    my ($type, $name) = @_;

    my %attributes = ();

    # Remove the UPPERCASE from the OCaml names, since OCaml does not allow
    # functions to have names with leading capital letters.
    if ($name =~ /^([A-Z]+)(.*)$/) {
        $attributes{"mlname(" . lc($1) . "_$2)"} = 1;
    }

    # If returning a char *, make it a string
    if ($type =~ /char\s+\*/) {
        $attributes{"string"} = 1;
        $attributes{"length_is(" . MAX_STRING_LENGTH . ")"} = 1;
    }

    # Function-specific special cases
    if (grep { /$name/ } (keys %manual_function_attributes)) {
        print "Manual function attribute(s) available for $name\n";
        # Add each manual attribute to the attribute record hash.
        my $function_attributes = $manual_function_attributes{$name}->{function_attributes};
        for (@$function_attributes) {
            $attributes{$_} = 1;
        }
    }

    (keys %attributes) ? (keys %attributes) : ();
}

=head2 maybe_adjust_return_type
Description:
If appropriate, change the return type of the given function so that "failure"
return values raise exceptions.

Arguments:
1. Original return type of the function.
2. Function name.

Returns:
Function return type that should be used.
=cut
sub maybe_adjust_return_type {
    my ($type, $name) = @_;

    if (grep { /$name/ } (keys %returns_error_code)) {
        print "Changing return type of $name to error code\n";
        return "HDF_RESULT";
    }
    else {
        return $type;
    }
}

=head2 argument_attributes
Description:
Determine appropriate attributes for function arguments.

Arguments:
1. Function name
2. Attribute type
3. Attribute name

Returns:
Array of argument attributes
OR
undef if there are no attributes for the given argument
=cut
sub argument_attributes {
    my ($function, $type, $name) = @_;

    my %attributes = ();

    # *get* and *info* and *inquire* are (generally?) using pointer values
    # as output.
    if ($function =~ /get|info|inquire/ and $type =~ /\*/) {
        $attributes{out} = 1;
    }

    # Make (const and un-const) char * arguments usable as strings.
    if ($type =~ /(?:const )?char \*/) {
        $attributes{string} = 1;
        if ($attributes{out}) {
            $attributes{"length_is(" . MAX_STRING_LENGTH . ")"} = 1;
        }
    }

    # Function-specific special cases
    if (grep { /$function/ } (keys %manual_function_attributes)) {
        if (grep { /$name/ } (keys %{$manual_function_attributes{$function}->{parameter_attributes}})) {
            print "Manual parameter attribute(s) available for $name\n";
            # Add each manual attribute to the attribute record hash.
            my $parameter_attributes = $manual_function_attributes{$function}->{parameter_attributes}->{$name};
            for (@$parameter_attributes) {
                $attributes{$_} = 1;
            }
        }
    }

    (keys %attributes) ? [keys %attributes] : undef;
}

=head2 process_prototype
Line-by-line processing of the function prototypes.
=cut
sub process_prototype {
    my $line = shift;
    my $bigarray = shift;
    $bigarray = $bigarray ? "bigarray, " : "";

    # There is probably a better way to do this...
    my ($return_type, $function_name, $args) = $line =~ /^((?:const )?\w+ (?:\*\s*)?)(\w+)\s*\(([\w\s\*\[\],]*)\)/;
    unless ($return_type and $function_name and $args) {
        print "$line\n";
    }
    minimize_whitespace ($return_type, $function_name, $args);
    $return_type = maybe_adjust_return_type($return_type, $function_name);

    # Hold attribute information for the whole prototype
    my @function_attributes = function_attributes $return_type, $function_name;

    # Argument list, split as a hash holding name and type for each (in order)
    my @arg_list = split ',', $args;
    my @function_args = map {
        my ($type, $name) = $_ =~ /(.*)?\b(\w+)/;
        minimize_whitespace ($type, $name);
        my %arg_hash = (name => $name, type => $type);
        my $attributes = argument_attributes $function_name, $type, $name;
        if ($attributes) {
            $arg_hash{attributes} = $attributes;
        }
        \%arg_hash
    } (split ',', $args);

    my @updated_arg_list = ();
    for my $arg (@function_args) {
        my @pieces = ($arg->{type}, $arg->{name});
        if (defined $arg->{attributes}) {
            @pieces = ((make_attribute_string @{$arg->{attributes}}), @pieces);
        }
        push @updated_arg_list, (join ' ', @pieces);
    }

    # Join it all back together, (hopefully) ready for camlidl processing.
    return
        (@function_attributes ? (make_attribute_string @function_attributes) . " " : "")
        . "$return_type $function_name("
        . (join ', ', @updated_arg_list) . ");";
}

my @input_lines = read_file ();
my $blob = join "", @input_lines;
my @scrubbed_lines = split "\n", clean_up $blob;

sub handle_file {
    my $bigarray = shift;
    my $output_filename = shift;

    my $OUTFILE;
    open $OUTFILE, ">$output_filename" or die "Error with output file: $!";

    my $is_a_typedef_struct = 0;
    my $is_a_typedef_enum = 0;

    for (@scrubbed_lines) {
        my $line = $_;
        SWITCH: {
            # Don't do processing inside of typedef (struct) definitions.
            # XXX - This really is a horrible hack, but it does the job.
            if (/^\s*typedef\s+struct[\s\w]*{/) { $is_a_typedef_struct = 1; last SWITCH; }
            if (/^\s*typedef.*?;/) { last SWITCH; }
            if (/}/ and $is_a_typedef_struct) { $is_a_typedef_struct = 0; last SWITCH; }
            if ($is_a_typedef_struct) { last SWITCH; }
            # If we make it to here, then we're not in a typedef, so we should process
            # the line.
            $line = process_prototype $_, $bigarray;
        }

        print $OUTFILE "$line\n";
    }

    close $OUTFILE;
}

handle_file 0, $output_file;
