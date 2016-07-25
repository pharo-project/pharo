pages
syntax
	"This method was automatically generated. Edit it using:"
	"RegexHelp edit: #syntax"
	^HelpTopic
		title: 'Syntax'
		contents: 
'[You can select and "print it'' examples in this method. Just don''t
forget to cancel the changes.]

The simplest regular expression is a single character.  It matches
exactly that character. A sequence of characters matches a string with
exactly the same sequence of characters:

	''a'' matchesRegex: ''a''				-- true
	''foobar'' matchesRegex: ''foobar''		-- true
	''blorple'' matchesRegex: ''foobar''		-- false

The above paragraph introduced a primitive regular expression (a
character), and an operator (sequencing). Operators are applied to
regular expressions to produce more complex regular expressions.
Sequencing (placing expressions one after another) as an operator is,
in a certain sense, `invisible''--yet it is arguably the most common.

A more `visible'' operator is Kleene closure, more often simply
referred to as `a star''.  A regular expression followed by an asterisk
matches any number (including 0) of matches of the original
expression. For example:

	''ab'' matchesRegex: ''a*b''		 		-- true
	''aaaaab'' matchesRegex: ''a*b''	 	-- true
	''b'' matchesRegex: ''a*b''		 		-- true
	''aac'' matchesRegex: ''a*b''	 		-- false: b does not match

A star''s precedence is higher than that of sequencing. A star applies
to the shortest possible subexpression that precedes it. For example,
''ab*'' means `a followed by zero or more occurrences of b'', not `zero
or more occurrences of ab'':

	''abbb'' matchesRegex: ''ab*''	 		-- true
	''abab'' matchesRegex: ''ab*''		 	-- false

To actually make a regex matching `zero or more occurrences of ab'',
`ab'' is enclosed in parentheses:

	''abab'' matchesRegex: ''(ab)*''		 	-- true
	''abcab'' matchesRegex: ''(ab)*''	 	-- false: c spoils the fun

Two other operators similar to `*'' are `+'' and `?''. `+'' (positive
closure, or simply `plus'') matches one or more occurrences of the
original expression. `?'' (`optional'') matches zero or one, but never
more, occurrences.

	''ac'' matchesRegex: ''ab*c''	 		-- true
	''ac'' matchesRegex: ''ab+c''	 		-- false: need at least one b
	''abbc'' matchesRegex: ''ab+c''		 	-- true
	''abbc'' matchesRegex: ''ab?c''		 	-- false: too many b''s

As we have seen, characters `*'', `+'', `?'', `('', and `)'' have special
meaning in regular expressions. If one of them is to be used
literally, it should be quoted: preceded with a backslash. (Thus,
backslash is also special character, and needs to be quoted for a
literal match--as well as any other special character described
further).

	''ab*'' matchesRegex: ''ab*''		 	-- false: star in the right string is special
	''ab*'' matchesRegex: ''ab\*''	 		-- true
	''a\c'' matchesRegex: ''a\\c''		 	-- true

The last operator is `|'' meaning `or''. It is placed between two
regular expressions, and the resulting expression matches if one of
the expressions matches. It has the lowest possible precedence (lower
than sequencing). For example, `ab*|ba*'' means `a followed by any
number of b''s, or b followed by any number of a''s'':

	''abb'' matchesRegex: ''ab*|ba*''	 	-- true
	''baa'' matchesRegex: ''ab*|ba*''	 	-- true
	''baab'' matchesRegex: ''ab*|ba*''	 	-- false

A bit more complex example is the following expression, matching the
name of any of the Lisp-style `car'', `cdr'', `caar'', `cadr'',
... functions:

	c(a|d)+r

It is possible to write an expression matching an empty string, for
example: `a|''.  However, it is an error to apply `*'', `+'', or `?'' to
such expression: `(a|)*'' is an invalid expression.

So far, we have used only characters as the ''smallest'' components of
regular expressions. There are other, more `interesting'', components.

A character set is a string of characters enclosed in square
brackets. It matches any single character if it appears between the
brackets. For example, `[01]'' matches either `0'' or `1'':

	''0'' matchesRegex: ''[01]''		 		-- true
	''3'' matchesRegex: ''[01]''		 		-- false
	''11'' matchesRegex: ''[01]''		 		-- false: a set matches only one character

Using plus operator, we can build the following binary number
recognizer:

	''10010100'' matchesRegex: ''[01]+''	 	-- true
	''10001210'' matchesRegex: ''[01]+''	 	-- false

If the first character after the opening bracket is `^'', the set is
inverted: it matches any single character *not* appearing between the
brackets:

	''0'' matchesRegex: ''[^01]''		  		-- false
	''3'' matchesRegex: ''[^01]''		 		-- true

For convenience, a set may include ranges: pairs of characters
separated with `-''. This is equivalent to listing all characters
between them: `[0-9]'' is the same as `[0123456789]''.

Special characters within a set are `^'', `-'', and `]'' that closes the
set. Below are the examples of how to literally use them in a set:

	[01^]		-- put the caret anywhere except the beginning
	[01-]		-- put the dash as the last character
	[]01]		-- put the closing bracket as the first character 
	[^]01]			(thus, empty and universal sets cannot be specified)

Regular expressions can also include the following backquote escapes
to refer to popular classes of characters:

	\w	any word constituent character (same as [a-zA-Z0-9_])
	\W	any character but a word constituent
	\d	a digit (same as [0-9])
	\D	anything but a digit
	\s 	a whitespace character (same as [:space:] below)
	\S	anything but a whitespace character

These escapes are also allowed in character classes: ''[\w+-]'' means
''any character that is either a word constituent, or a plus, or a
minus''.

Character classes can also include the following grep(1)-compatible
elements to refer to:

	[:alnum:]		any alphanumeric character (same as [a-zA-Z0-9])
	[:alpha:]		any alphabetic character (same as [a-zA-Z])
	[:cntrl:]		any control character. (any character with code < 32)
	[:digit:]		any decimal digit (same as [0-9])
	[:graph:]		any graphical character. (any character with code >= 32).
	[:lower:]		any lowercase character (including non-ASCII lowercase characters)
	[:print:]		any printable character. In this version, this is the same as [:graph:]
	[:punct:]		any punctuation character:  . , !! ? ; : '' - ( ) ` and double quotes
	[:space:]		any whitespace character (space, tab, CR, LF, null, form feed, Ctrl-Z, 16r2000-16r200B, 16r3000)
	[:upper:]		any uppercase character (including non-ASCII uppercase characters)
	[:xdigit:]		any hexadecimal character (same as [a-fA-F0-9]).

Note that many of these are only as consistent or inconsistent on issues
of locale as the underlying Smalltalk implementation. Values shown here
are for VisualWorks 7.6.

Note that these elements are components of the character classes,
i.e. they have to be enclosed in an extra set of square brackets to
form a valid regular expression.  For example, a non-empty string of
digits would be represented as ''[[:digit:]]+''.

The above primitive expressions and operators are common to many
implementations of regular expressions. The next primitive expression
is unique to this Smalltalk implementation.

A sequence of characters between colons is treated as a unary selector
which is supposed to be understood by Characters. A character matches
such an expression if it answers true to a message with that
selector. This allows a more readable and efficient way of specifying
character classes. For example, `[0-9]'' is equivalent to `:isDigit:'',
but the latter is more efficient. Analogously to character sets,
character classes can be negated: `:^isDigit:'' matches a Character
that answers false to #isDigit, and is therefore equivalent to
`[^0-9]''.

As an example, so far we have seen the following equivalent ways to
write a regular expression that matches a non-empty string of digits:

	''[0-9]+''
	''\d+''
	''[\d]+''
	''[[:digit:]]+''
	:isDigit:+''

The last group of special primitive expressions includes: 

	.	matching any character except a NULL; 
	^	matching an empty string at the beginning of a line; 
	$	matching an empty string at the end of a line.
	\b	an empty string at a word boundary
	\B	an empty string not at a word boundary
	\<	an empty string at the beginning of a word
	\>	an empty string at the end of a word

	''axyzb'' matchesRegex: ''a.+b''		-- true
	''ax zb'' matchesRegex: ''a.+b''			-- true (space is matched by `.'')
	''ax
zb'' matchesRegex: ''a.+b''				-- true (carriage return is matched by `.'')

Again, the dot ., caret ^ and dollar $ characters are special and should be quoted
to be matched literally.

	EXAMPLES

As the introductions said, a great use for regular expressions is user
input validation. Following are a few examples of regular expressions
that might be handy in checking input entered by the user in an input
field. Try them out by entering something between the quotes and
print-iting. (Also, try to imagine Smalltalk code that each validation
would require if coded by hand).  Most example expressions could have
been written in alternative ways.

Checking if aString may represent a nonnegative integer number:

	'''' matchesRegex: '':isDigit:+''
or
	'''' matchesRegex: ''[0-9]+''
or
	'''' matchesRegex: ''\d+''

Checking if aString may represent an integer number with an optional
sign in front:

	'''' matchesRegex: ''(\+|-)?\d+''

Checking if aString is a fixed-point number, with at least one digit
is required after a dot:

	'''' matchesRegex: ''(\+|-)?\d+(\.\d+)?''

The same, but allow notation like `123.'':

	'''' matchesRegex: ''(\+|-)?\d+(\.\d*)?''

Recognizer for a string that might be a name: one word with first
capital letter, no blanks, no digits.  More traditional:

	'''' matchesRegex: ''[A-Z][A-Za-z]*''

more Smalltalkish:

	'''' matchesRegex: '':isUppercase::isAlphabetic:*''

A date in format MMM DD, YYYY with any number of spaces in between, in
XX century:

	'''' matchesRegex: ''(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[ ]+(\d\d?)[ ]*,[ ]*19(\d\d)''

Note parentheses around some components of the expression above. As
`Usage'' section shows, they will allow us to obtain the actual strings
that have matched them (i.e. month name, day number, and year number).

For dessert, coming back to numbers: here is a recognizer for a
general number format: anything like 999, or 999.999, or -999.999e+21.

	'''' matchesRegex: ''(\+|-)?\d+(\.\d*)?((e|E)(\+|-)?\d+)?''
!' readStream nextChunkText