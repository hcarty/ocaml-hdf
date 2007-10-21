#!/usr/bin/perl

use strict;
use warnings;

use constant MAX_STRING_LENGTH => 1024;

my $input_file = $ARGV[0];
my $output_file = $ARGV[0] . ".inc";

=begin
Read in the file contents, return an array containing each of the lines.
=cut
sub read_file {
    my $file = $input_file;
    open my $fin, $file or die "Unable to open $file for reading: $!";
    my @lines = <$fin>;
    close $fin;
    return @lines;
}

=begin
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

    (keys %attributes) ? (keys %attributes) : ();
}

sub argument_attributes {
    my ($function, $type, $name) = @_;

    my %attributes = ();

    # *get* and *info* are (generally?) using pointer values as output.
    if ($function =~ /get|info/ and $type =~ /\*/) {
        $attributes{out} = 1;
    }

    # Make (const and un-const) char * arguments usable as strings.
    if ($type =~ /(?:const )?char \*/) {
        $attributes{string} = 1;
        if ($attributes{out}) {
            $attributes{"length_is(" . MAX_STRING_LENGTH . ")"} = 1;
        }
    }

    (keys %attributes) ? [keys %attributes] : undef;
}

=begin
Line-by-line processing of the function prototypes.
=cut
sub process_prototype {
    my $line = shift;
    my $bigarray = shift;
    $bigarray = $bigarray ? "bigarray, " : "";

    # There is probably a better way to do this...
    my ($return_type, $function_name, $args) = $line =~ /^(\w+ (?:\*\s*)?)(\w+)\s*\(([\w\s\*\[\],]*)\)/;
    unless ($return_type and $function_name and $args) {
        print "$line\n";
    }
    minimize_whitespace ($return_type, $function_name, $args);

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
