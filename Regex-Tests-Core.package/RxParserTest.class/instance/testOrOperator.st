tests
testOrOperator
	"self debug: #testOrOperator"
	
	"The last operator is `|' meaning `or'. It is placed between two
regular expressions, and the resulting expression matches if one of
the expressions matches. It has the lowest possible precedence (lower
than sequencing). For example, `ab*|ba*' means `a followed by any
number of b's, or b followed by any number of a's':"

	self assert: ('abb' matchesRegex: 'ab*|ba*').  	
	self assert: ('baa' matchesRegex: 'ab*|ba*').	 	
	self deny: ('baab' matchesRegex: 'ab*|ba*').
	

	"It is possible to write an expression matching an empty string, for
example: `a|'.  However, it is an error to apply `*', `+', or `?' to
such expression: `(a|)*' is an invalid expression."

	self should: ['(a|)*' asRegex] raise: Error.
