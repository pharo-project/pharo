"
Small pieces of source code used used to test various AST based tools.

See `RBCodeSnippet allSnippets` for a various collection of instances.

* source <String> the source code of the snippet
* isMethod <Boolean> the source is for a full method (with pattern and maybe pragmas)
* formatedCode <String> the expected reformated code
* isParseFaulty <Boolean> is the parser expected to produce a faulty AST (only syntactic errors)
* isFaulty <Boolean> is the compiler expected to produce a faulty AST (syntactic and semantic errors)
* value <Object> the expected value when executed
* hasValue <Boolean> is the compiled method produce a value
* raise <Exception class> the exception that should be raised at runtime
* messageNotUnderstood <Symbol> the expected MNU error when executed
* numberOfCritique <Integer> how many critiques are expected
* skippedTests <Collection> list of test to not execute (that should be fixed)

Tests are executed by the parametrized matrix class `RBCodeSnippetTest`

Additional tools can add more information and expectation (please improve the list and the tools)
"
Class {
	#name : #RBCodeSnippet,
	#superclass : #Object,
	#instVars : [
		'source',
		'isMethod',
		'isParseFaulty',
		'isFaulty',
		'notices',
		'value',
		'hasValue',
		'formattedCode',
		'skippedTests',
		'raise',
		'messageNotUnderstood',
		'numberOfCritiques',
		'group',
		'default'
	],
	#category : #'AST-Core-Tests-Snippets'
}

{ #category : #accessing }
RBCodeSnippet class >> allSnippets [

	^ {
		  self badExpressions.
		  self badMethods.
		  self badSemantic } flattened
]

{ #category : #accessing }
RBCodeSnippet class >> badExpressions [
	"This list contains various (and possibly systematic) variations of faulty inputs (and some correct ones for good measure)."

	"Unless specifically testing token handling (e.g. in the scanner) try to use the formater format `formattedCode` as the source to simplify this file"

	| list |
	list := {
		        (self new
			         source: '#';
			         notices: #( #( 1 1 2 'Literal expected' ) )).
		        (self new
			         source: '$';
			         notices: #( #( 1 1 2 'Character expected' ) )).
		        (self new
			         source: ':';
			         notices: #( #( 1 1 1 'Variable name expected' ) )).
		        (self new
			         source: '';
			         isFaulty: false). "emptyness is ok"

		        "Comments"
		        "EFFormater is not the best here :("
		        (self new
			         source: '"" ';
			         isFaulty: false).
		        (self new
			         source: '"nothing" ';
			         isFaulty: false).
		        (self new
			         source: '"com"1"ment"';
			         formattedCode: '1';
			         isFaulty: false;
			         value: 1). "The comments are in the AST, the formatter just do not know to show them because we format only the node and not the whole method body"
		        (self new
			         source: '"a" 1 "b". "c" 2 "d"';
			         formattedCode: '1. "a" "b" "c" 2 "d"';
			         isFaulty: false;
			         value: 2). "a and b moved around. Formatter issue. FIXME?"
		        (self new
			         source: '"unfinished';
			         notices: #( #( 1 11 12 'Unmatched " in comment.' ) )).
		        (self new
			         source: '"also unfinished""';
			         notices: #( #( 1 18 19 'Unmatched " in comment.' ) )).
		        (self new
			         source: '"';
			         notices: #( #( 1 1 2 'Unmatched " in comment.' ) )).
		        (self new
			         source: '"""';
			         notices: #( #( 1 3 4 'Unmatched " in comment.' ) )).

		        "Bad compound"
		        (self new
			         source: '( 1 + 2';
			         notices: #( #( 1 7 8 ''')'' expected' ) )).
		        (self new
			         source: '#( 1 + 2';
			         notices: #( #( 1 8 9 ''')'' expected' ) )).
		        (self new
			         source: '[ 1 + 2';
			         notices: #( #( 1 7 8 ''']'' expected' ) )).
		        (self new
			         source: '#[ 1 2';
			         notices: #( #( 1 6 7 ''']'' expected' ) )).
		        (self new
			         source: '{ 1 + 2';
			         notices: #( #( 1 7 8 '''}'' expected' ) )).
		        (self new
			         source: '1 + 2 )';
			         notices: #( #( 1 7 7 'Missing opener for closer: )' ) )).
		        (self new
			         source: '1 + 2 ]';
			         notices: #( #( 1 7 7 'Missing opener for closer: ]' ) )).
		        (self new
			         source: '1 + 2 }';
			         notices: #( #( 1 7 7 'Missing opener for closer: }' ) )).
		        (self new
			         source: '( ';
			         notices: #( #( 3 2 3 'Variable or expression expected' )
				            #( 1 2 3 ''')'' expected' ) )).
		        (self new
			         source: '#( ';
			         notices: #( #( 1 2 4 ''')'' expected' ) )).
		        (self new
			         source: '[ ';
			         notices: #( #( 1 1 3 ''']'' expected' ) )).
		        (self new
			         source: '#[ ';
			         notices: #( #( 1 2 4 ''']'' expected' ) )).
		        (self new
			         source: '{ ';
			         notices: #( #( 1 1 3 '''}'' expected' ) )).
		        (self new
			         source: '{ [ ( ';
			         notices: #( #( 7 6 7 'Variable or expression expected' )
				            #( 5 6 7 ''')'' expected' )
				            #( 3 6 7 ''']'' expected' )
				            #( 1 6 7 '''}'' expected' ) )).
		        (self new
			         source: ') ] }';
			         notices:
				         #( #( 1 1 1 'unexpected token' )
				            #( 1 3 3 'Missing opener for closer: ]' )
				            #( 1 5 5 'Missing opener for closer: }' ) )).

		        "Compounds with an unexped thing inside"
		        (self new
			         source: '(1]2)';
			         formattedCode: '( 1 ]. 2 )';
			         notices:
				         #( #( 1 2 3 ''')'' expected' )
				            #( 1 3 3 'Missing opener for closer: ]' )
				            #( 4 5 5 'Missing opener for closer: )' ) )).
		        (self new
			         source: '(1}2)';
			         formattedCode: '( 1 }. 2 )';
			         notices:
				         #( #( 1 2 3 ''')'' expected' )
				            #( 1 3 3 'Missing opener for closer: }' )
				            #( 4 5 5 'Missing opener for closer: )' ) )).
		        (self new
			         source: '(1. 2)';
			         formattedCode: '( 1. 2 )';
			         notices:
				         #( #( 1 2 3 ''')'' expected' )
				            #( 5 6 6 'Missing opener for closer: )' ) )).
		        (self new
			         source: '[1)2]';
			         formattedCode: '[ 1 ). 2 ]';
			         notices: #( #( 2 3 3 'Missing opener for closer: )' ) )).
		        (self new
			         source: '[1}2]';
			         formattedCode: '[ 1 }. 2 ]';
			         notices: #( #( 2 3 3 'Missing opener for closer: }' ) )).
		        (self new
			         source: '#(1]2}3)';
			         formattedCode: '#( 1 #'']'' 2 #''}'' 3 )';
			         isFaulty: false;
			         value: #( 1 #']' 2 #'}' 3 )). "`#(` can eat almost anything"
		        (self new
			         source: '#( 0 1r2 4 )';
			         formattedCode: '#( 0 1 r2 4 )';
			         notices:
				         #( #( 6 6 7 'an integer greater than 1 as valid radix expected' )
				            #( 1 12 13 ''')'' expected' ) )). "Almost anything..."
		        (self new
			         source: '#[ 1 ) 2 ]';
			         notices: #( #( 6 6 6 '8-bit integer expected' )
				            #( 1 10 11 ''']'' expected' ) )).
		        (self new
			         source: '#[ 1 } 2 ]';
			         notices: #( #( 6 6 6 '8-bit integer expected' )
				            #( 1 10 11 ''']'' expected' ) )).
		        (self new
			         source: '#[ 1 a 2 ]';
			         notices: #( #( 6 6 6 '8-bit integer expected' )
				            #( 1 10 11 ''']'' expected' ) )).
		        (self new
			         source: '#[ 1 -1 2 ]';
			         notices: #( #( 6 7 6 '8-bit integer expected' )
				            #( 1 11 12 ''']'' expected' ) )).
		        (self new
			         source: '#[ 1 1.0 2 ]';
			         notices: #( #( 6 8 6 '8-bit integer expected' )
				            #( 1 12 13 ''']'' expected' ) )).
		        (self new
			         source: '#[ 1 256 2 ]';
			         notices: #( #( 6 8 6 '8-bit integer expected' )
				            #( 1 12 13 ''']'' expected' ) )).
		        (self new
			         source: '{1)2}';
			         formattedCode: '{ 1 ). 2 }';
			         notices: #( #( 2 3 3 'Missing opener for closer: )' ) )).
		        (self new
			         source: '{1]2}';
			         formattedCode: '{ 1 ]. 2 }';
			         notices: #( #( 2 3 3 'Missing opener for closer: ]' ) )).

		        "...or without expected thing"
		        "Note: all compounds `[]` `#()` `#[]` `{}` are legal empty, except one"
		        (self new
			         source: '()';
			         notices: #( #( 2 1 2 'Variable or expression expected' ) )).

		        "Bad sequence. The first expression is considered unfinished."
		        (self new
			         source: '1 2';
			         formattedCode: '1. 2';
			         notices: #( #( 1 2 3 'End of statement expected' ) )).
		        (self new
			         source: '1 foo 2';
			         formattedCode: '1 foo. 2';
			         notices: #( #( 1 6 7 'End of statement expected' ) )).
		        (self new
			         source: '(1)2';
			         formattedCode: '1. 2';
			         notices: #( #( 1 3 4 'End of statement expected' ) )).
		        (self new
			         source: '1(2)';
			         formattedCode: '1. 2';
			         notices: #( #( 1 1 2 'End of statement expected' ) )).
		        (self new
			         source: '(1)(2)';
			         formattedCode: '1. 2';
			         notices: #( #( 1 3 4 'End of statement expected' ) )).
		        (self new
			         source: '#hello#world';
			         formattedCode: '#hello. #world';
			         notices: #( #( 1 6 7 'End of statement expected' ) )).
		        (self new
			         source: '$h$w';
			         formattedCode: '$h. $w';
			         notices: #( #( 1 2 3 'End of statement expected' ) )).
		        (self new
			         source: '[1][2]';
			         formattedCode: '[ 1 ]. [ 2 ]';
			         notices: #( #( 1 3 4 'End of statement expected' ) )).
		        (self new
			         source: '{1}{2}';
			         formattedCode: '{ 1 }. { 2 }';
			         notices: #( #( 1 3 4 'End of statement expected' ) )).
		        (self new
			         source: '#(1)#(2)';
			         formattedCode: '#( 1 ). #( 2 )';
			         notices: #( #( 1 4 5 'End of statement expected' ) )).
		        (self new
			         source: '#[1]#[2]';
			         formattedCode: '#[ 1 ]. #[ 2 ]';
			         notices: #( #( 1 4 5 'End of statement expected' ) )).

		        "Bad temporary variable declarations"
		        "Note: bad temporaries will be stored as error statements"
		        (self new
			         source: '| ';
			         notices: #( #( 1 1 3 '''|'' or variable expected' ) )).
		        (self new
			         source: '| a b';
			         notices:
				         #( #( 3 3 3 'Undeclared variable' )
				            #( 5 5 5 'Undeclared variable' )
				            #( 1 5 6 '''|'' or variable expected' ) )).
		        (self new
			         source: '| 1';
			         formattedCode: '| . 1';
			         notices: #( #( 1 1 3 '''|'' or variable expected' ) )).
		        "Note that the | character is also a binary operator, so a missing opening | become a binary call with a missing argument (see bellow)"
		        (self new
			         source: 'a | ';
			         notices:
				         #( #( 1 1 1 'Undeclared variable' )
				            #( 5 4 5 'Variable or expression expected' ) )).
		        (self new
			         source: 'a || ';
			         notices:
				         #( #( 1 1 1 'Undeclared variable' )
				            #( 6 5 6 'Variable or expression expected' ) )).
		        (self new
			         source: '| | a';
			         formattedCode: 'a';
			         isFaulty: false;
			         notices: #( #( 5 5 5 'Undeclared variable' ) )). "This one is legal, it is an empty list of temporaries, the | are dismissed"
		        (self new
			         source: '|| a';
			         formattedCode: 'a';
			         isFaulty: false;
			         notices: #( #( 4 4 4 'Undeclared variable' ) )). "Same, but are messing with the Scanner"
		        (self new
			         source: ' ||| a';
			         formattedCode: ' | a';
			         notices: #( #( 4 3 4 'Variable or expression expected' )
				            #( 6 6 6 'Undeclared variable' ) )). "this one is a empty temps and a binary operator | with a mising receiver"
		        (self new
			         source: ' |||| a';
			         formattedCode: ' || a';
			         notices: #( #( 4 3 4 'Variable or expression expected' )
				            #( 7 7 7 'Undeclared variable' ) )). "this one is a empty temps and a binary operator || with a mising receiver"
		        (self new
			         source: '| a | | b';
			         notices:
				         #( #( 3 3 3 'unused variable' )
				            #( 7 6 7 'Variable or expression expected' )
				            #( 9 9 9 'Undeclared variable' ) )). "A valid temporary followed by a binary operator with a missing receiver"
		        (self new
			         source: '| a ||b';
			         formattedCode: '| a | | b';
			         notices:
				         #( #( 3 3 3 'unused variable' )
				            #( 6 5 6 'Variable or expression expected' )
				            #( 7 7 7 'Undeclared variable' ) )). "same"

		        "Unexpected parameters (or columns)"
		        "Note that `:a` is not a token but a special `:` followed by an identifier, whereas `a:` is a single token."
		        "Nevertheless, the parser will try to catch unexpected :a together"
		        (self new
			         source: ':a';
			         notices: #( #( 1 2 1 'Variable name expected' ) )).
		        (self new
			         source: '::a';
			         notices: #( #( 1 3 1 'Variable name expected' ) )).
		        (self new
			         source: ':::a';
			         notices: #( #( 1 4 1 'Variable name expected' ) )).
		        (self new
			         source: '::';
			         notices: #( #( 1 2 1 'Variable name expected' ) )).
		        (self new
			         source: ':a foo';
			         notices: #( #( 1 2 1 'Variable name expected' ) )).
		        (self new
			         source: 'a :foo';
			         formattedCode: 'a. :foo';
			         notices:
				         #( #( 1 1 1 'Undeclared variable' )
				            #( 1 2 3 'End of statement expected' )
				            #( 3 6 3 'Variable name expected' ) )).
		        (self new
			         source: 'a : foo';
			         formattedCode: 'a. :foo';
			         notices:
				         #( #( 1 1 1 'Undeclared variable' )
				            #( 1 2 3 'End of statement expected' )
				            #( 3 7 3 'Variable name expected' ) )).
		        (self new
			         source: 'a:';
			         formattedCode: ' a: ';
			         notices: #( #( 1 0 1 'Variable or expression expected' )
				            #( 3 2 3 'Variable or expression expected' ) )). "keyword message with a missing receiver and argument"
		        (self new
			         source: 'a::';
			         notices: #( #( 1 3 1 'unexpected token' ) )). "just a bad token"
		        (self new
			         source: 'a:foo';
			         formattedCode: ' a: foo';
			         notices: #( #( 1 0 1 'Variable or expression expected' )
				            #( 3 5 3 'Undeclared variable' ) )). "keyword message with a missing receiver"
		        (self new
			         source: 'a::foo';
			         formattedCode: 'a::. foo';
			         notices:
				         #( #( 1 3 1 'unexpected token' )
				            #( 4 6 4 'Undeclared variable' ) )).
		        (self new
			         source: ':a:foo';
			         formattedCode: ': a: foo';
			         notices: #( #( 1 3 1 'Variable name expected' )
				            #( 4 6 4 'Undeclared variable' ) )).
		        (self new
			         source: '|:a|';
			         formattedCode: '| . :a | ';
			         notices: #( #( 1 1 2 '''|'' or variable expected' )
				            #( 2 3 2 'Variable name expected' )
				            #( 5 4 5 'Variable or expression expected' ) )).
		        (self new
			         source: '|:a';
			         formattedCode: '| . :a';
			         notices: #( #( 1 1 2 '''|'' or variable expected' )
				            #( 2 3 2 'Variable name expected' ) )).
		        (self new
			         source: '|::a';
			         formattedCode: '| . ::a';
			         notices: #( #( 1 1 2 '''|'' or variable expected' )
				            #( 2 4 2 'Variable name expected' ) )).
		        (self new
			         source: '|a:|';
			         formattedCode: '| . a: | ';
			         notices: #( #( 1 1 2 '''|'' or variable expected' )
				            #( 2 1 2 'Variable or expression expected' )
				            #( 4 3 4 'Variable or expression expected' )
				            #( 5 4 5 'Variable or expression expected' ) )).
		        (self new
			         source: '|a:';
			         formattedCode: '| . a: ';
			         notices: #( #( 1 1 2 '''|'' or variable expected' )
				            #( 2 1 2 'Variable or expression expected' )
				            #( 4 3 4 'Variable or expression expected' ) )).

		        "Bad block parameters"
		        "A bad parameter cause a error object to be added as the last element of the block parameter.
		On formating, a double space can be seen."
		        (self new
			         source: '[:a b]';
			         formattedCode: '[ :a | b ]';
			         value: nil;
			         notices: #( #( 5 4 5 '''|'' or parameter expected' )
				            #( 5 5 5 'Undeclared variable' ) )). "FIXME"
		        (self new
			         source: '[:a 1]';
			         formattedCode: '[ :a | 1 ]';
			         value: 1;
			         notices: #( #( 5 4 5 '''|'' or parameter expected' ) )). "FIXME"
		        (self new
			         source: '[:a :]';
			         formattedCode: '[ :a : | ]';
			         value: nil;
			         notices: #( #( 6 5 6 'Variable name expected' ) )). "FIXME"
		        (self new
			         source: '[:a ::b]';
			         formattedCode: '[ :a ::b | ]';
			         value: nil;
			         notices: #( #( 6 7 6 'Variable name expected' ) )). "FIXME"
		        (self new
			         source: '[:a :b]';
			         formattedCode: '[ :a :b | ]';
			         isFaulty: false). "no pipe (and no body) is legal"
		        (self new
			         source: '[: a : b]';
			         formattedCode: '[ :a :b | ]';
			         isFaulty: false). "spaces are also legal"
		        (self new
			         source: '[:a:b]';
			         formattedCode: '[ : | a: b ]';
			         notices: #( #( 3 2 3 'Variable name expected' )
				            #( 3 2 3 '''|'' or parameter expected' )
				            #( 3 2 3 'Name already defined' )
				            #( 3 2 3 'Variable or expression expected' )
				            #( 5 5 5 'Undeclared variable' ) )). "FIXME?"
		        (self new
			         source: '[ a: ]';
			         notices: #( #( 3 2 3 'Variable or expression expected' )
				            #( 6 5 6 'Variable or expression expected' ) )). "no parameters, a keyword message send witout receiver nor arguments"
		        (self new
			         source: '[ | ]';
			         notices: #( #( 3 3 5 '''|'' or variable expected' ) )).
		        (self new
			         source: '[ | b ]';
			         notices:
				         #( #( 5 5 5 'Undeclared variable' )
				            #( 3 5 7 '''|'' or variable expected' ) )).
		        (self new
			         source: '[ :a | | b ]';
			         notices: #( #( 10 10 10 'Undeclared variable' )
				            #( 8 10 12 '''|'' or variable expected' ) )).
		        (self new
			         source: '[ :a || b ]';
			         formattedCode: '[ :a | | b ]';
			         notices:
				         #( #( 9 9 9 'Undeclared variable' )
				            #( 7 9 11 '''|'' or variable expected' ) )).
		        (self new
			         source: '[:a| | |b]';
			         formattedCode: '[ :a | b ]';
			         isFaulty: false;
			         notices: #( #( 9 9 9 'Undeclared variable' ) )). "Explicit empty list of temporaries"
		        (self new
			         source: '[:a| ||b]';
			         formattedCode: '[ :a | b ]';
			         isFaulty: false;
			         notices: #( #( 8 8 8 'Undeclared variable' ) )). "Same but mess with the Scanner"
		        (self new
			         source: '[:a|| |b]';
			         formattedCode: '[ :a | b ]';
			         isFaulty: false;
			         notices: #( #( 8 8 8 'Undeclared variable' ) )). "Same"
		        (self new
			         source: '[:a|||b]';
			         formattedCode: '[ :a | b ]';
			         isFaulty: false;
			         notices: #( #( 7 7 7 'Undeclared variable' ) )). "Same"
		        (self new
			         source: '[:a||||b]';
			         formattedCode: '[ :a | | b ]';
			         notices: #( #( 7 6 7 'Variable or expression expected' )
				            #( 8 8 8 'Undeclared variable' ) )). "same + binary | without receiver"

		        "Unclosed blocks"
		        (self new
			         source: '[ : | ';
			         notices: #( #( 1 1 7 ''']'' expected' ) )).
		        (self new
			         source: '[:';
			         formattedCode: '[ : | ';
			         notices: #( #( 1 1 3 ''']'' expected' ) )).
		        (self new
			         source: '[ :a :b | ';
			         notices: #( #( 1 1 11 ''']'' expected' ) )).
		        (self new
			         source: '[ :a :b';
			         formattedCode: '[ :a :b | ';
			         notices: #( #( 1 1 8 ''']'' expected' ) )).
		        (self new
			         source: '[ :a b';
			         formattedCode: '[ :a | b';
			         notices:
				         #( #( 6 6 6 'Undeclared variable' )
				            #( 1 6 7 ''']'' expected' ) )).
		        (self new
			         source: '[ :a | ';
			         notices: #( #( 1 1 8 ''']'' expected' ) )).
		        (self new
			         source: '[ :a | b';
			         notices:
				         #( #( 8 8 8 'Undeclared variable' )
				            #( 1 8 9 ''']'' expected' ) )).
		        (self new
			         source: '[ | ';
			         notices: #( #( 3 3 5 '''|'' or variable expected' )
				            #( 1 3 5 ''']'' expected' ) )).
		        (self new
			         source: '[ | 1';
			         notices: #( #( 3 3 5 '''|'' or variable expected' )
				            #( 1 5 6 ''']'' expected' ) )).
		        (self new
			         source: '[ | a';
			         notices:
				         #( #( 5 5 5 'Undeclared variable' )
				            #( 3 5 6 '''|'' or variable expected' )
				            #( 1 5 6 ''']'' expected' ) )).

		        "Missing receiver or argument in message sends.
		Note: a unary message send without a receiver will be 'correctly' mistaken as a variable, so not a parsing error"
		        "binary"
		        (self new
			         source: ' + ';
			         notices: #( #( 2 1 2 'Variable or expression expected' )
				            #( 4 3 4 'Variable or expression expected' ) )).
		        (self new
			         source: '1 + ';
			         notices: #( #( 5 4 5 'Variable or expression expected' ) )).
		        (self new
			         source: ' + 2';
			         notices: #( #( 2 1 2 'Variable or expression expected' ) )).
		        "keywords"
		        (self new
			         source: ' hello: ';
			         notices: #( #( 2 1 2 'Variable or expression expected' )
				            #( 9 8 9 'Variable or expression expected' ) )).
		        (self new
			         source: '1 hello: ';
			         notices:
				         #( #( 10 9 10 'Variable or expression expected' ) )).
		        (self new
			         source: ' hello: 2';
			         notices: #( #( 2 1 2 'Variable or expression expected' ) )).
		        (self new
			         source: ' goodby: my: ';
			         notices: #( #( 2 1 2 'Variable or expression expected' )
				            #( 10 9 10 'Variable or expression expected' )
				            #( 14 13 14 'Variable or expression expected' ) )).
		        (self new
			         source: '1 goodby: my: ';
			         notices:
				         #( #( 11 10 11 'Variable or expression expected' )
				            #( 15 14 15 'Variable or expression expected' ) )).
		        (self new
			         source: '1 goodby: 2 my: ';
			         notices:
				         #( #( 17 16 17 'Variable or expression expected' ) )).
		        (self new
			         source: ' goodby: 2 my: ';
			         notices: #( #( 2 1 2 'Variable or expression expected' )
				            #( 16 15 16 'Variable or expression expected' ) )).
		        (self new
			         source: ' goodby: my: 3';
			         notices: #( #( 2 1 2 'Variable or expression expected' )
				            #( 10 9 10 'Variable or expression expected' ) )).
		        (self new
			         source: '1 goodby: my: 3';
			         notices:
				         #( #( 11 10 11 'Variable or expression expected' ) )).
		        (self new
			         source: ' goodby: 2 my: 3';
			         notices: #( #( 2 1 2 'Variable or expression expected' ) )).
		        "Combinaisons"
		        (self new
			         source: ' + foo: - ';
			         notices: #( #( 2 1 2 'Variable or expression expected' )
				            #( 4 3 4 'Variable or expression expected' )
				            #( 9 8 9 'Variable or expression expected' )
				            #( 11 10 11 'Variable or expression expected' ) )).

		        "Bad assignments"
		        (self new
			         source: 'a := ';
			         notices: #( #( 6 5 6 'Variable or expression expected' )
				            #( 1 1 1 'Undeclared variable' ) )).
		        (self new
			         source: ':= ';
			         notices: #( #( 4 3 4 'Variable or expression expected' )
				            #( 1 3 1 'variable expected in assigment' ) )).
		        (self new
			         source: ':= 2';
			         notices: #( #( 1 4 1 'variable expected in assigment' ) )).
		        (self new
			         source: '1:=2';
			         formattedCode: '1. := 2';
			         notices: #( #( 1 1 2 'End of statement expected' )
				            #( 2 4 2 'variable expected in assigment' ) )).

		        "Bad cascades"
		        (self new
			         source: ';';
			         formattedCode: ' ; ';
			         notices: #( #( 1 0 1 'Variable or expression expected' )
				            #( 1 0 1 'Message expected' )
				            #( 2 1 2 'Cascade message expected' ) )).
		        (self new
			         source: '1;foo';
			         formattedCode: '1 ; foo';
			         notices: #( #( 1 1 2 'Message expected' ) )).
		        (self new
			         source: '1;';
			         formattedCode: '1 ; ';
			         notices:
				         #( #( 1 1 2 'Message expected' )
				            #( 3 2 3 'Cascade message expected' ) )).
		        (self new
			         source: '1 sign;';
			         formattedCode: '1 sign; ';
			         notices: #( #( 8 7 8 'Cascade message expected' ) )).
		        (self new
			         source: '1 foo:;bar';
			         formattedCode: '1 foo: ; bar';
			         notices: #( #( 7 6 7 'Variable or expression expected' ) )). "The cascade is correct here. It's a simple error of a missing argument"
		        (self new
			         source: '1 foo;2';
			         formattedCode: '1 foo; . 2';
			         notices: #( #( 7 6 7 'Cascade message expected' )
				            #( 1 6 7 'End of statement expected' ) )).
		        (self new
			         source: '(1 sign: 2);bar';
			         formattedCode: '(1 sign: 2) ; bar';
			         notices: #( #( 1 11 12 'Message expected' ) )).
		        (self new
			         source: '(1 sign);bar';
			         formattedCode: '1 sign ; bar';
			         notices: #( #( 1 8 9 'Message expected' ) )). "FIXME the parentheses are lost, and this changes the meaning"
		        "Longer cascade"
		        (self new
			         source: ';;';
			         formattedCode: ' ; ; ';
			         notices: #( #( 1 0 1 'Variable or expression expected' )
				            #( 1 0 1 'Message expected' )
				            #( 2 1 2 'Cascade message expected' )
				            #( 3 2 3 'Cascade message expected' ) )).
		        (self new
			         source: '1 sign;;bar';
			         formattedCode: '1 sign; ; bar';
			         notices: #( #( 8 7 8 'Cascade message expected' ) )).

		        "Bad returns"
		        (self new
			         source: '^ ';
			         notices: #( #( 3 2 3 'Variable or expression expected' ) )).
		        (self new
			         source: '1+^2';
			         formattedCode: '1 + . ^ 2';
			         notices: #( #( 3 2 3 'Variable or expression expected' )
				            #( 1 2 3 'End of statement expected' ) )).
		        (self new
			         source: '1 foo: ^2';
			         formattedCode: '1 foo: . ^ 2';
			         notices: #( #( 8 7 8 'Variable or expression expected' )
				            #( 1 7 8 'End of statement expected' ) )).
		        (self new
			         source: '(^1)';
			         formattedCode: '( . ^ 1 )';
			         notices: #( #( 2 1 2 'Variable or expression expected' )
				            #( 1 1 2 ''')'' expected' )
				            #( 2 4 4 'Missing opener for closer: )' ) )). "^ can only appear a the begin of a statement, so a random ^ cause an unfinished statement error"
		        (self new
			         source: '^^1';
			         formattedCode: '^ . ^ 1';
			         notices: #( #( 2 1 2 'Variable or expression expected' )
				            #( 1 1 2 'End of statement expected' ) )). "Same spirit"
		        (self new
			         source: '[ ^ 1 ]';
			         isFaulty: false;
			         raise: BlockCannotReturn). "when the block is evaluated, the method is already gone."
		        (self new
			         source: '{ ^ 1 }';
			         isFaulty: false;
			         value: 1). "I did not expect this one to be legal"
		        (self new
			         source: '#(^1)';
			         formattedCode: '#( #''^'' 1 )';
			         isFaulty: false;
			         value: #( #'^' 1 )). "Obviously..."
		        (self new
			         source: '#[ ^ 1 ]';
			         notices: #( #( 4 4 4 '8-bit integer expected' )
				            #( 1 8 9 ''']'' expected' ) )).

		        "Unreachable code (warnings)"
		        "Unreachable analysis is very simple and targets statement that directly follows a return statement."
		        "Note that faulty code can still be executed without a RuntimeSyntaxError"
		        (self new
			         source: '^ 1. 2. ^ 3';
			         isFaulty: false;
			         value: 1;
			         notices: #( #( 6 6 6 'Unreachable statement' ) )).
		        (self new
			         source: '[ ^ 1. 2. ^ 3 ]';
			         isFaulty: false;
			         raise: BlockCannotReturn;
			         notices: #( #( 8 8 8 'Unreachable statement' ) )). "like [^1]"
		        (self new
			         source: '{ ^ 1. 2. ^ 3 }';
			         isFaulty: false;
			         value: 1;
			         notices: #( #( 8 8 8 'Unreachable statement' ) )).
		        (self new
			         source: '[ ^ 1 ]. 2. ^ 3';
			         isFaulty: false;
			         value: 3).
		        (self new
			         source: '{ ^ 1 }. 2. ^ 3';
			         isFaulty: false;
			         value: 1). "This one could have been..."
		        (self new
			         source: 'true ifTrue: [ ^ 1 ] ifFalse: [ ^ 2 ]. ^ 3';
			         isFaulty: false;
			         value: 1). "Not *syntactic* enough"

		        "Bad string literal"
		        "Note: the only cases are the missing closing quotes since everything inside is captured as is and there is no escape sequences or interpolation (yet?)"
		        (self new
			         source: '''hello';
			         notices: #( #( 1 6 7 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '''hello''''world';
			         notices:
				         #( #( 1 13 14 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '''';
			         notices: #( #( 1 1 2 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '''hello''''';
			         notices: #( #( 1 8 9 'Unmatched '' in string literal.' ) )). "unclosed string that ends with an escaped quote"

		        "Bad symbol literal"
		        (self new
			         source: '#1';
			         formattedCode: '#. 1';
			         notices: #( #( 1 1 2 'Literal expected' ) )). "Become a bad sequence"
		        (self new
			         source: '#1r0';
			         formattedCode: '#. 1 r0';
			         notices:
				         #( #( 1 1 2 'Literal expected' )
				            #( 2 2 3 'an integer greater than 1 as valid radix expected' ) )). "Two bad sequences"
		        (self new
			         source: '##';
			         formattedCode: '#';
			         notices: #( #( 1 2 3 'Literal expected' ) )). "errr. ok?"
		        "Note: if quotes, same thing than strings"
		        (self new
			         source: '#''hello';
			         notices: #( #( 1 7 8 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '#''hello''''world';
			         notices:
				         #( #( 1 14 15 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '#''';
			         notices: #( #( 1 2 3 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '#''hello''''';
			         notices:
				         #( #( 1 9 10 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '###''hello';
			         notices:
				         #( #( 1 9 10 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '###''hello''''world';
			         notices:
				         #( #( 1 16 17 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '###''';
			         notices: #( #( 1 4 5 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '###''hello''''';
			         notices:
				         #( #( 1 11 12 'Unmatched '' in string literal.' ) )).

		        "Bad numeric literal"
		        "Note: currently there is only 2 cases or bad numeric literal, both related to bad radix"
		        (self new
			         source: '2r';
			         notices:
				         #( #( 1 2 3 'a digit between 0 and 1 expected' ) )).
		        (self new
			         source: '2rx';
			         formattedCode: '2r x';
			         notices:
				         #( #( 1 2 3 'a digit between 0 and 1 expected' ) )). "a bad number followed by a unary message send"
		        (self new
			         source: '2r3';
			         formattedCode: '2r. 3';
			         notices:
				         #( #( 1 2 3 'a digit between 0 and 1 expected' ) )). "a bad number followed by a number, causing a case of unfinished sequence"
		        (self new
			         source: '0r';
			         formattedCode: '0 r';
			         notices:
				         #( #( 1 1 2 'an integer greater than 1 as valid radix expected' ) )).
		        (self new
			         source: '000rx';
			         formattedCode: '000 rx';
			         notices:
				         #( #( 1 3 4 'an integer greater than 1 as valid radix expected' ) )).
		        (self new
			         source: '000r1';
			         formattedCode: '000 r1';
			         notices:
				         #( #( 1 3 4 'an integer greater than 1 as valid radix expected' ) )).
		        (self new
			         source: '3r12345';
			         formattedCode: '3r12. 345';
			         notices: #( #( 1 4 5 'End of statement expected' ) )).

		        "These ones are correct, the number parser is very prermisive (except for radix, see above)"
		        (self new
			         source: '1.';
			         formattedCode: '1';
			         isFaulty: false;
			         value: 1).
		        (self new
			         source: '1.1.1';
			         formattedCode: '1.1. 1';
			         isFaulty: false;
			         value: 1).
		        (self new
			         source: '1a';
			         formattedCode: '1 a';
			         isFaulty: false;
			         messageNotUnderstood: #a).
		        (self new
			         source: '1a1a1';
			         formattedCode: '1 a1a1';
			         isFaulty: false;
			         messageNotUnderstood: #a1a1).
		        (self new
			         source: '1e';
			         formattedCode: '1 e';
			         isFaulty: false;
			         messageNotUnderstood: #e).
		        (self new
			         source: '1e1e1';
			         formattedCode: '1e1 e1';
			         isFaulty: false;
			         messageNotUnderstood: #e1).
		        (self new
			         source: '1s';
			         isFaulty: false;
			         value: 1s0). "ScaledDecimal is a thing (!) that have literals (!!) inconsistent with '1e' (!!!)"
		        (self new
			         source: '1s1s1';
			         formattedCode: '1s1 s1';
			         isFaulty: false;
			         messageNotUnderstood: #s1).
		        (self new
			         source: '10r89abcd';
			         formattedCode: '10r89 abcd';
			         isFaulty: false;
			         messageNotUnderstood: #abcd).
		        (self new
			         source: '12r89abcd';
			         formattedCode: '12r89ab cd';
			         isFaulty: false;
			         messageNotUnderstood: #cd).
		        (self new
			         source: '36r1halt';
			         isFaulty: false;
			         value: 2486513). "ahah"

		        "Bad characters"
		        "Pharo is Unicode aware."
		        "$Δ isLetter >>> true" "$Δ asInteger >>> 16r0394" "Greek Capital Letter Delta"
		        "$ə isLetter >>> true" "$ə asInteger >>> 16r0259" "Latin Small Letter Schwa"
		        (self new
			         source: 'Δə';
			         isFaulty: false;
			         notices: #( #( 1 2 1 'Undeclared variable' ) )). "valid identifier"
		        (self new
			         source: '| Δə | Δə := 1. Δə + 1';
			         isFaulty: false;
			         value: 2).

		        "$± isSpecial >>> true" "$± asInteger >>> 16r00B1" "Plus Minus Sign" "Only a few unicode characters are isSpecial in Pharo"
		        (self new
			         source: '1 ± 1';
			         isFaulty: false;
			         messageNotUnderstood: #±). "Valid binary operator, but not implemented"
		        "$→ isSpecial >>> false" "$→ asInteger hex >>> 16r2192" "Rightwards Arrow"
		        (self new
			         source: '→';
			         notices: #( #( 1 0 1 'Unknown character' ) )). "Unknown character. Not isSpecial"
		        "$٠ isDigit >>> true" "$٠ asInteger >>> 16r0660" "Arabic-indic Digit Zero"
		        (self new
			         source: '٠';
			         notices: #( #( 1 0 1 'Unknown character' ) )). "Unknown character. Not a valid number (basic ASCII only for numbers!)"
		        "Currently in Pharo, there is no 'isSeparator' character outside the ASCII range"
		        "Character nbsp isSeparator >>> false" "Even the standard nbsp"
		        (self new source: Character nbsp asString). "Unknown character. Not isSeparator"

		        (self new
			         source: '$→';
			         isFaulty: false;
			         value: $→).
		        (self new
			         source: '''Δ→ə''';
			         isFaulty: false;
			         value: 'Δ→ə').
		        (self new
			         source: '"Δ→ə" ';
			         isFaulty: false).
		        (self new
			         source: '#''Δ→ə''';
			         isFaulty: false;
			         value: #'Δ→ə').
		        (self new
			         source: '#Δə';
			         formattedCode: '#''Δə''';
			         isFaulty: false;
			         value: #'Δə').
		        (self new
			         source: '#(Δ→ə)';
			         formattedCode: '#( Δ → ə )';
			         notices:
				         #( #( 4 3 4 'Unknown character' )
				            #( 1 6 7 ''')'' expected' ) )). "This one is faulty because → is a parse error"
		        (self new
			         source: '#→';
			         formattedCode: '#. →';
			         notices:
				         #( #( 1 1 2 'Literal expected' )
				            #( 2 1 2 'Unknown character' ) )) "Two independent errors" }.
	"Setup default values"
	self new
		group: #badExpressions;
		isMethod: false;
		isFaulty: true;
		applyDefaultTo: list.
	^ list
]

{ #category : #accessing }
RBCodeSnippet class >> badMethods [

	| list |
	list := {
		        (self new
			         source: ' ';
			         notices: #( #( 2 1 2 'Message pattern expected' ) )).

		        "Wrong token for pattern"
		        "An empty pattern will be set, and the first statement will be a error node."
		        (self new
			         source: '5';
			         formattedCode: ' . 5';
			         notices: #( #( 1 0 1 'Message pattern expected' ) )).
		        (self new
			         source: '''hello''';
			         formattedCode: ' . ''hello''';
			         notices: #( #( 1 0 1 'Message pattern expected' ) )).
		        (self new
			         source: '#hello';
			         formattedCode: ' . #hello';
			         notices: #( #( 1 0 1 'Message pattern expected' ) )).
		        (self new
			         source: ':';
			         formattedCode: ' . :';
			         notices: #( #( 1 0 1 'Message pattern expected' )
				            #( 1 1 1 'Variable name expected' ) )).
		        (self new
			         source: '#(foo bar)';
			         formattedCode: ' . #( foo bar )';
			         notices: #( #( 1 0 1 'Message pattern expected' ) )).

		        "Random bad token"
		        "Tought: is complainng about the bad token really better than complaining about the bad pattern?"
		        (self new
			         source: ' $';
			         notices: #( #( 2 2 3 'Character expected' ) )).
		        (self new
			         source: ' ''hello';
			         notices: #( #( 2 7 8 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: ' 2r3';
			         formattedCode: ' 2r. 3';
			         notices:
				         #( #( 2 3 4 'a digit between 0 and 1 expected' ) )).

		        "Bad aruments"
		        "The missing argument will be an error, the remainer starts the body"
		        (self new
			         source: '+ ';
			         value: nil;
			         notices: #( #( 3 2 3 'Variable name expected' ) )).
		        (self new
			         source: '+ 1';
			         value: nil;
			         notices: #( #( 3 2 3 'Variable name expected' ) )).
		        (self new
			         source: '+ foo: ';
			         notices: #( #( 3 2 3 'Variable name expected' )
				            #( 3 2 3 'Variable or expression expected' )
				            #( 8 7 8 'Variable or expression expected' ) )).
		        (self new
			         source: 'foo: ';
			         value: nil;
			         notices: #( #( 6 5 6 'Variable name expected' ) )).
		        (self new
			         source: 'foo: 1';
			         value: nil;
			         notices: #( #( 6 5 6 'Variable name expected' ) )).
		        (self new
			         source: 'foo: + ';
			         notices: #( #( 6 5 6 'Variable name expected' )
				            #( 6 5 6 'Variable or expression expected' )
				            #( 8 7 8 'Variable or expression expected' ) )).
		        (self new
			         source: 'foo: bar: ';
			         value: nil;
			         notices: #( #( 6 5 6 'Variable name expected' )
				            #( 11 10 11 'Variable name expected' )
				            #( 11 10 11 'Name already defined' ) )).
		        (self new
			         source: 'foo:bar:';
			         formattedCode: ' . foo:bar:';
			         notices: #( #( 1 0 1 'Message pattern expected' )
				            #( 1 8 1 'unexpected token' ) )). "`foo:bar:` is a single token, and is unexpected"

		        "Bad pragma message"
		        (self new
			         source: 'foo < ';
			         value: nil;
			         notices: #( #( 7 6 7 'Message pattern expected' )
				            #( 5 6 7 '''>'' expected' ) )).
		        (self new
			         source: 'foo <> ';
			         notices: #( #( 5 4 5 'Variable or expression expected' )
				            #( 8 7 8 'Variable or expression expected' ) )).
		        (self new
			         source: 'foo < 4';
			         value: nil;
			         notices: #( #( 7 6 7 'Message pattern expected' )
				            #( 5 6 7 '''>'' expected' ) )).
		        (self new
			         source: 'foo < bar ';
			         value: nil;
			         notices: #( #( 5 10 11 '''>'' expected' ) )).
		        (self new
			         source: 'foo < bar: ';
			         value: nil;
			         notices: #( #( 12 11 12 'Literal constant expected' )
				            #( 5 11 12 '''>'' expected' ) )).
		        (self new
			         source: 'foo < bar: 1 1 > ';
			         notices:
				         #( #( 5 13 14 '''>'' expected' )
				            #( 18 17 18 'Variable or expression expected' ) )).
		        (self new
			         source: 'foo < bar ; baz > ';
			         formattedCode: 'foo < bar ; baz. > ';
			         notices:
				         #( #( 5 10 11 '''>'' expected' )
				            #( 11 10 11 'Variable or expression expected' )
				            #( 11 10 11 'Message expected' )
				            #( 11 10 11 'Variable or expression expected' )
				            #( 11 16 17 'End of statement expected' )
				            #( 17 16 17 'Variable or expression expected' )
				            #( 19 18 19 'Variable or expression expected' ) )).

		        "Bad pragma value"
		        (self new
			         source: 'foo <bar: > ';
			         value: nil;
			         notices: #( #( 11 10 11 'Literal constant expected' ) )).
		        (self new
			         source: 'foo <bar:(1)>';
			         formattedCode: 'foo < bar: 1 > ';
			         notices: #( #( 10 9 10 'Literal constant expected' )
				            #( 5 9 10 '''>'' expected' )
				            #( 14 13 14 'Variable or expression expected' ) )). "FIXME. dont eat parentheses"
		        (self new
			         source: 'foo < bar: baz > ';
			         notices: #( #( 12 11 12 'Literal constant expected' )
				            #( 5 11 12 '''>'' expected' )
				            #( 12 14 12 'Undeclared variable' )
				            #( 18 17 18 'Variable or expression expected' ) )).
		        (self new
			         source: 'foo < bar: 1 + 1 > ';
			         notices:
				         #( #( 5 13 14 '''>'' expected' )
				            #( 14 13 14 'Variable or expression expected' )
				            #( 20 19 20 'Variable or expression expected' ) )).
		        (self new
			         source: 'foo < bar: [ 1 ] > ';
			         notices: #( #( 12 11 12 'Literal constant expected' )
				            #( 5 11 12 '''>'' expected' )
				            #( 20 19 20 'Variable or expression expected' ) )).
		        (self new
			         source: 'foo < bar: { 1 } > ';
			         notices: #( #( 12 11 12 'Literal constant expected' )
				            #( 5 11 12 '''>'' expected' )
				            #( 20 19 20 'Variable or expression expected' ) )).
		        (self new
			         source: 'foo <bar: #[ -1 ]> ';
			         value: nil;
			         notices: #( #( 14 15 14 '8-bit integer expected' )
				            #( 11 17 18 ''']'' expected' ) )). "Literal bytes arrays are acceptable, but this one is faulty"
		        (self new
			         source: 'foo < + 1> ';
			         isFaulty: false). "Binary message is legal pragma"
		        (self new
			         source: 'foo < + > ';
			         value: nil;
			         notices: #( #( 9 8 9 'Literal constant expected' ) )) }.
	"Setup default values"
	self new
		group: #badMethods;
		isMethod: true;
		isFaulty: true;
		applyDefaultTo: list.
	^ list
]

{ #category : #accessing }
RBCodeSnippet class >> badSemantic [
	"List of varions semantic and backend errors."

	| list |
	"Undefined variable
	For some reason, this is just a warning in non-faulty mode, not an error."
	list := {
		        (self new
			         source: 'a := 10. ^ a';
			         value: 10;
			         notices:
				         #( #( 1 1 1 'Undeclared variable' )
				            #( 12 12 12 'Undeclared variable' ) );
			         skip: #exec). "a is bound to an undefined variable, and according to some compilation options it could be registerer or not registered. This is very bad as regitred ones act like globals thus polute other tests. So disable the execution for now."
		        (self new
			         source: '^ a';
			         notices: #( #( 3 3 3 'Undeclared variable' ) )).

		        "Uninitialized variable"
		        (self new
			         source: '| a | ^ a';
			         notices: #( #( 9 9 9 'unitialized variable' ) )).
		        (self new source: '| a | [ a := 10 ]. ^ a').
		        (self new
			         source: '| a | [ ^ a ]. a := 10';
			         value: 10;
			         notices: #( #( 11 11 11 'unitialized variable' ) )).

		        "Duplicated variable definition (same scope)"
		        (self new
			         source: 'foo: a bar: a ^ a';
			         isMethod: true;
			         value: 1;
			         notices: #( #( 13 13 13 'Name already defined' ) )).
		        (self new
			         source: '| a a | a := 10. ^ a';
			         value: 10;
			         notices:
				         #( #( 3 3 3 'unused variable' )
				            #( 5 5 5 'Name already defined' ) )).
		        (self new
			         source: '[ | a a | a := 10. a ]';
			         value: 10;
			         notices:
				         #( #( 5 5 5 'unused variable' )
				            #( 7 7 7 'Name already defined' ) )).
		        (self new
			         source: '[ :a :a | a ]';
			         value: 1;
			         notices: #( #( 7 7 7 'Name already defined' ) )).

		        "Shadowed variables"
		        (self new
			         source: 'foo: a ^ [ | a | a := 10. a ] value + a';
			         isMethod: true;
			         value: 11;
			         notices: #( #( 14 14 14 'Name already defined' ) )).
		        (self new
			         source:
				         'foo | a | a := 1. ^ [ | a | a := 10. a ] value + a';
			         isMethod: true;
			         value: 11;
			         notices: #( #( 25 25 25 'Name already defined' ) )).
		        (self new
			         source:
				         'foo ^ [ | a | a := 1. [ | a | a := 10. a ] value + a ] value';
			         isMethod: true;
			         value: 11;
			         notices: #( #( 27 27 27 'Name already defined' ) )).
		        (self new
			         source:
				         'foo ^ [ :a | [ | a | a := 10. a ] value + a ] value: 1';
			         isMethod: true;
			         value: 11;
			         notices: #( #( 18 18 18 'Name already defined' ) )).
		        (self new
			         source: 'foo: a ^ [ :a | a ] value: 10 + a';
			         isMethod: true;
			         value: 11;
			         notices: #( #( 13 13 13 'Name already defined' ) )).
		        (self new
			         source: 'foo | a | a := 1. ^ [ :a | a ] value: 10 + a';
			         isMethod: true;
			         value: 11;
			         notices: #( #( 24 24 24 'Name already defined' ) )).
		        (self new
			         source:
				         'foo ^ [ | a | a := 1. [ :a | a ] value: 10 + a ] value';
			         isMethod: true;
			         value: 11;
			         notices: #( #( 26 26 26 'Name already defined' ) )).
		        (self new
			         source: 'foo ^ [ :a | [ :a | a ] value: 10 + a ] value: 1';
			         isMethod: true;
			         value: 11;
			         notices: #( #( 17 17 17 'Name already defined' ) )). "phonyArgs"

		        "Write on readonly or reserved"
		        (self new
			         source: 'foo: a a := 10. ^ a';
			         isMethod: true;
			         isFaulty: true;
			         notices:
				         #( #( 8 8 8 'Assignment to read-only variable' ) )).
		        (self new
			         source: '[ :a | a := 10. a ]';
			         isFaulty: true;
			         notices:
				         #( #( 8 8 8 'Assignment to read-only variable' ) )).
		        "The following assignment are explicitely no-op, to minimize the impact if, for some broken reasons, they are really executed"
		        (self new
			         source: 'nil := nil';
			         formattedCode: 'nil. := nil';
			         isFaulty: true;
			         isParseFaulty: true;
			         notices: #( #( 1 4 5 'End of statement expected' )
				            #( 5 10 5 'variable expected in assigment' ) )). "it is a literal, not an identifier"
		        (self new
			         source: 'true := true';
			         formattedCode: 'true. := true';
			         isFaulty: true;
			         isParseFaulty: true;
			         notices: #( #( 1 5 6 'End of statement expected' )
				            #( 6 12 6 'variable expected in assigment' ) )). "it is a literal, not an identifier"
		        (self new
			         source: 'false := false';
			         formattedCode: 'false. := false';
			         isFaulty: true;
			         isParseFaulty: true;
			         notices: #( #( 1 6 7 'End of statement expected' )
				            #( 7 14 7 'variable expected in assigment' ) )). "it is a literal, not an identifier"
		        (self new
			         source: 'self := self';
			         isFaulty: true;
			         numberOfCritiques: 1;
			         notices: #( #( 1 4 1 'Assigment to reserved variable' )
				            #( 1 4 1 'Assignment to read-only variable' ) )).
		        (self new
			         source: 'super := super';
			         isFaulty: true;
			         numberOfCritiques: 1;
			         notices: #( #( 1 5 1 'Assigment to reserved variable' )
				            #( 1 5 1 'Assignment to read-only variable' ) )).
		        (self new
			         source: 'thisContext := thisContext';
			         isFaulty: true;
			         numberOfCritiques: 1;
			         notices: #( #( 1 11 1 'Assigment to reserved variable' )
				            #( 1 11 1 'Assignment to read-only variable' ) )).
		        (self new
			         source: 'Object := Object';
			         isFaulty: true;
			         numberOfCritiques: 1;
			         notices:
				         #( #( 1 6 1 'Assignment to read-only variable' ) )).

		        "Shadowed reserved or global"
		        (self new
			         source: '| self | self := 1. ^ self';
			         value: 1;
			         notices: #( #( 3 6 3 'Name already defined' ) )).
		        (self new
			         source: '| super | super := 1. ^ super';
			         value: 1;
			         notices: #( #( 3 7 3 'Name already defined' ) )).
		        (self new
			         source: '| thisContext | thisContext := 1. ^ thisContext';
			         value: 1;
			         notices: #( #( 3 13 3 'Name already defined' ) )).
		        (self new
			         source: '| Object | Object := 1. ^ Object';
			         value: 1;
			         notices: #( #( 3 8 3 'Name already defined' ) )).
		        (self new
			         source: 'foo: self ^ self + 1';
			         isMethod: true;
			         value: 2;
			         notices: #( #( 6 9 6 'Name already defined' ) )).
		        (self new
			         source: 'foo: super ^ super + 1';
			         isMethod: true;
			         value: 2;
			         notices: #( #( 6 10 6 'Name already defined' ) )).
		        (self new
			         source: 'foo: thisContext ^ thisContext + 1';
			         isMethod: true;
			         value: 2;
			         notices: #( #( 6 16 6 'Name already defined' ) )).
		        (self new
			         source: 'foo: Object ^ Object + 1';
			         isMethod: true;
			         value: 2;
			         notices: #( #( 6 11 6 'Name already defined' ) )).
		        (self new
			         source: '[ :self | self + 1 ]';
			         value: 2;
			         notices: #( #( 4 7 4 'Name already defined' ) )).
		        (self new
			         source: '[ :super | super + 1 ]';
			         value: 2;
			         notices: #( #( 4 8 4 'Name already defined' ) )).
		        (self new
			         source: '[ :thisContext | thisContext + 1 ]';
			         value: 2;
			         notices: #( #( 4 14 4 'Name already defined' ) )).
		        (self new
			         source: '[ :Object | Object + 1 ]';
			         value: 2;
			         notices: #( #( 4 9 4 'Name already defined' ) )).

		        "Backend errors"
		        "FIXME: syntax error are thrown by the compiler even in *faulty* mode.
		 FIXME: semantic analysis does not catch the error (backend issue)"
		        (self new
			         source:
				         'foo ^ [ :a1 :a2 :a3 :a4 :a5 :a6 :a7 :a8 :a9 :a10 :a11 :a12 :a13 :a14 :a15 :a16 | a1 ]';
			         isMethod: true;
			         isFaulty: true;
			         notices: #( #( 7 85 7 'Too many arguments' ) )). "Too many arguments"
		        (self new
			         source:
				         'a1: a1 a2: a2 a3: a3 a4: a4 a5: a5 a6: a6 a7: a7 a8: a8 a9: a9 a10: a10 a11: a11 a12: a12 a13: a13 a14: a14 a15: a15 a16: a16 ^ a1';
			         isMethod: true;
			         isFaulty: true;
			         notices: #( #( 1 130 1 'Too many arguments' ) )) "Too many arguments" }.
	"Setup default values"
	self new
		group: #badSemantic;
		isMethod: false;
		isFaulty: false;
		isParseFaulty: false;
		applyDefaultTo: list.
	^ list
]

{ #category : #script }
RBCodeSnippet class >> styleAll [
	"Display all snippets in a big styled text.
	Each snipped is styled independently."

	<script>
	| bigtext |
	bigtext := Text new.
	self allSnippets do: [ :each |
		bigtext ifNotEmpty: [ bigtext append: String cr ].
		bigtext append: each styledText ].
	bigtext inspect
]

{ #category : #script }
RBCodeSnippet class >> styleAllWithError [
	"Display all snippets in a big styled text with compilation error messages and error nodes.
	Each compilation error message and each error node is displayed indenpendently."

	<script>
	| bigtext |
	bigtext := Text new.
	self allSnippets do: [ :each |
		| text ast |
		bigtext ifNotEmpty: [ bigtext append: String cr ].

		"Get the original styled text"
		text := each styledText.
		bigtext append: text.

		"Try to compile it and collect error messages"
		each compileOnError: [ :exception |
				"In case of error, insert error message (like an old-school smalltalk compiler)"
				text
					replaceFrom: exception location
					to: exception location - 1
					with: (exception messageText asText addAttribute:
							 (TextBackgroundColor color: Color lightBlue)).
				bigtext append: String cr.
				bigtext append: String tab.
				bigtext append: text.
				bigtext append: '   '.
				bigtext append: ('SyntaxErrorNotification' asText makeAllColor: Color gray).
				].

		"Then the fun part. Add line for each node with error or warning"
		ast := each doSemanticAnalysis.
		ast nodesPostorderDo: [ :node |
			node notices do: [ :notice |
				text := each textWithNode: node message: notice messageText at: notice position.
				bigtext append: String cr.
				bigtext append: String tab.
				bigtext append: text.
				bigtext append: (('   {1} ({2})' format: {node class name. notice class name}) asText makeAllColor: Color gray)
			] ] ].
	bigtext inspect
]

{ #category : #accessing }
RBCodeSnippet >> applyDefaultTo: aCollection [

	aCollection do: [ :each |
		each default: self.
		each isMethod ifNil: [ each isMethod: self isMethod ].
		each isParseFaulty ifNil: [ each isParseFaulty: self isParseFaulty ].
		each isFaulty ifNil: [ each isFaulty: self isFaulty ].
		each formattedCode ifNil: [ each formattedCode: each source ].
	]
]

{ #category : #accessing }
RBCodeSnippet >> default: aRBCodeSnippet [

	default := aRBCodeSnippet
]

{ #category : #accessing }
RBCodeSnippet >> formattedCode [

	^ formattedCode
]

{ #category : #accessing }
RBCodeSnippet >> formattedCode: anObject [

	formattedCode := anObject
]

{ #category : #accessing }
RBCodeSnippet >> group [
	^ group
]

{ #category : #accessing }
RBCodeSnippet >> group: aString [
	group := aString
]

{ #category : #testing }
RBCodeSnippet >> hasNotice: aString at: anInteger [

	^ self notices anySatisfy: [ :each |
		  each third = anInteger and: [ each fourth = aString ] ]
]

{ #category : #accessing }
RBCodeSnippet >> hasValue [

	^ hasValue ifNil: [ value isNotNil | isFaulty not ]
]

{ #category : #accessing }
RBCodeSnippet >> hasValue: anObject [

	hasValue := anObject
]

{ #category : #asserting }
RBCodeSnippet >> ifSkip: aTestName then: aBlock [

	skippedTests ifNil: [ ^ self ].
	(skippedTests includes: aTestName) ifFalse: [ ^ self ].
	^ aBlock value
]

{ #category : #accessing }
RBCodeSnippet >> isFaulty [

	^ isFaulty
]

{ #category : #accessing }
RBCodeSnippet >> isFaulty: anObject [

	isFaulty := anObject
]

{ #category : #accessing }
RBCodeSnippet >> isMethod [

	^ isMethod
]

{ #category : #accessing }
RBCodeSnippet >> isMethod: anObject [

	isMethod := anObject
]

{ #category : #accessing }
RBCodeSnippet >> isParseFaulty [

	^ isParseFaulty
]

{ #category : #accessing }
RBCodeSnippet >> isParseFaulty: anObject [

	isParseFaulty := anObject
]

{ #category : #accessing }
RBCodeSnippet >> messageNotUnderstood [

	^ messageNotUnderstood
]

{ #category : #accessing }
RBCodeSnippet >> messageNotUnderstood: anObject [

	messageNotUnderstood := anObject
]

{ #category : #accessing }
RBCodeSnippet >> notices [
	^ notices
]

{ #category : #accessing }
RBCodeSnippet >> notices: aCollection [
	notices := aCollection
]

{ #category : #accessing }
RBCodeSnippet >> numberOfCritiques [

	^ numberOfCritiques
]

{ #category : #accessing }
RBCodeSnippet >> numberOfCritiques: anObject [

	numberOfCritiques := anObject
]

{ #category : #parsing }
RBCodeSnippet >> parse [
	^ isMethod
		ifTrue: [ RBParser parseFaultyMethod: self source ]
		ifFalse: [ RBParser parseFaultyExpression: self source ]
]

{ #category : #parsing }
RBCodeSnippet >> parseOnError: aBlock [

	^ [ isMethod
			  ifTrue: [ RBParser parseMethod: self source ]
			  ifFalse: [ RBParser parseExpression: self source ] ]
		  on: CodeError
		  do: [ :e | aBlock value: e ]
]

{ #category : #printing }
RBCodeSnippet >> printOn: aStream [

	super printOn: aStream.
	source ifNotNil: [
		aStream
			nextPut: $(;
			nextPutAll: source;
			nextPut: $) ]
]

{ #category : #accessing }
RBCodeSnippet >> raise [

	^ raise
]

{ #category : #accessing }
RBCodeSnippet >> raise: anObject [

	raise := anObject
]

{ #category : #accessing }
RBCodeSnippet >> skip: aSymbol [

	skippedTests ifNil: [ skippedTests := OrderedCollection new ].
	skippedTests add: aSymbol
]

{ #category : #accessing }
RBCodeSnippet >> skippedTests [

	^ skippedTests
]

{ #category : #accessing }
RBCodeSnippet >> skippedTests: anObject [

	skippedTests := anObject
]

{ #category : #accessing }
RBCodeSnippet >> source [

	^ source
]

{ #category : #accessing }
RBCodeSnippet >> source: anObject [

	source := anObject
]

{ #category : #inspecting }
RBCodeSnippet >> textWithNode: aNode message: aString at: aPosition [

	| text |
	text := self styledText.
	"Highlight the error node background"
	text
		addAttribute: (TextBackgroundColor color: Color cyan)
		from: aNode start
		to: aNode stop.
	text
		replaceFrom: aPosition
		to: aPosition - 1
		with: (aString asText addAttribute:
				 (TextBackgroundColor color: Color lightBlue)).
	^ text
]

{ #category : #accessing }
RBCodeSnippet >> value [

	^ value
]

{ #category : #accessing }
RBCodeSnippet >> value: anObject [

	value := anObject.
	hasValue := true
]
