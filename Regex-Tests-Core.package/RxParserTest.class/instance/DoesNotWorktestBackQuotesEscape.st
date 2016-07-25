tests
DoesNotWorktestBackQuotesEscape
	"self debug: #testBackQuotesEscape"
	
	"Regular expressions can also include the following backquote escapes
to refer to popular classes of characters:
	\w	any word constituent character (same as [a-zA-Z0-9:=])
	\W	any character but a word constituent
	\d	a digit (same as [0-9])
	\D	anything but a digit
	\s 	a whitespace character
	\S	anything but a whitespace character
These escapes are also allowed in character classes: '[\w+-]' means
'any character that is either a word constituent, or a plus, or a
minus'."
	
	self assert: ('one word' matchesRegex: '\w').	
		
	self assert: ('one' matchesRegex: '\w').		  	
	