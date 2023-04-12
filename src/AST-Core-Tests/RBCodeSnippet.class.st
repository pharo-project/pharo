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
		'isFaultyMinusUndeclared',
		'notices',
		'value',
		'hasValue',
		'formattedCode',
		'skippedTests',
		'raise',
		'messageNotUnderstood',
		'numberOfCritiques',
		'group',
		'default',
		'nodePositions'
	],
	#category : #'AST-Core-Tests-Snippets'
}

{ #category : #accessing }
RBCodeSnippet class >> allSnippets [

	^ {
		  self badSimpleExpressions.
		  self badExpressions.
		  self badTokens.
		  self badMethods.
		  self badSemantic.
		  self badPositions.
		  self badVariableAndScopes } flattened
]

{ #category : #accessing }
RBCodeSnippet class >> badExpressions [
	"This list focuses on composed expressions and sequences of statements.
	It contains various (and possibly systematic) variations of faulty inputs (and some correct ones for good measure).
	Unless specifically testing token handling (e.g. in the scanner) try to use the formater format `formattedCode` as the source to simplify this file"

	<script: 'self styleWithError: self badExpressions'>
	| list |
	list := {
		        (self new
			         source: '( 1 + 2';
			         nodeAt: '0021113';
			         notices: #( #( 1 7 8 ''')'' expected' ) )).
		        (self new
			         source: '#( 1 + 2';
			         nodeAt: '00010203';
			         notices: #( #( 1 8 9 ''')'' expected' ) )).
		        (self new
			         source: '[ 1 + 2';
			         nodeAt: '0032224';
			         notices: #( #( 1 7 8 ''']'' expected' ) )).
		        (self new
			         source: '#[ 1 2';
			         nodeAt: '000102';
			         notices: #( #( 1 6 7 ''']'' expected' ) )).
		        (self new
			         source: '{ 1 + 2';
			         nodeAt: '0021113';
			         notices: #( #( 1 7 8 '''}'' expected' ) )).
		        (self new
			         source: '1 + 2 )';
			         nodeAt: '2111300';
			         notices: #( #( 1 7 7 'Missing opener for closer: )' ) )).
		        (self new
			         source: '1 + 2 ]';
			         nodeAt: '2111300';
			         notices: #( #( 1 7 7 'Missing opener for closer: ]' ) )).
		        (self new
			         source: '1 + 2 }';
			         nodeAt: '2111300';
			         notices: #( #( 1 7 7 'Missing opener for closer: }' ) )).
		        (self new
			         source: '( ';
			         nodeAt: '00';
			         notices: #( #( 3 2 3 'Variable or expression expected' )
				            #( 1 2 3 ''')'' expected' ) )).
		        (self new
			         source: '#( ';
			         nodeAt: '00';
			         notices: #( #( 1 2 4 ''')'' expected' ) )).
		        (self new
			         source: '[ ';
			         nodeAt: '00';
			         notices: #( #( 1 2 3 ''']'' expected' ) )).
		        (self new
			         source: '#[ ';
			         nodeAt: '00';
			         notices: #( #( 1 2 4 ''']'' expected' ) )).
		        (self new
			         source: '{ ';
			         nodeAt: '0';
			         notices: #( #( 1 1 3 '''}'' expected' ) )).
		        (self new
			         source: '{ [ ( ';
			         nodeAt: '001133';
			         notices: #( #( 7 6 7 'Variable or expression expected' )
				            #( 5 6 7 ''')'' expected' )
				            #( 3 6 7 ''']'' expected' )
				            #( 1 6 7 '''}'' expected' ) )).
		        (self new
			         source: ' )';
			         nodeAt: ' 0';
			         notices: #( #( 2 2 2 'Missing opener for closer: )' ) )).
		        (self new
			         source: ' ]';
			         nodeAt: ' 0';
			         notices: #( #( 2 2 2 'Missing opener for closer: ]' ) );
			         skip: #testCodeImporter). "FIXME. code importer do not like rogue starting `]`"
		        (self new
			         source: ' }';
			         nodeAt: ' 0';
			         notices: #( #( 2 2 2 'Missing opener for closer: }' ) )).
		        (self new
			         source: ' ) ] }';
			         nodeAt: ' 21100';
			         notices: #( #( 2 2 2 'Missing opener for closer: )' )
				            #( 2 4 4 'Missing opener for closer: ]' )
				            #( 2 6 6 'Missing opener for closer: }' ) )).

		        "Compounds with an unexped thing inside"
		        (self new
			         source: '(1]2)';
			         nodeAt: '23154';
			         formattedCode: '( 1 ]. 2 )';
			         notices:
				         #( #( 1 2 3 ''')'' expected' )
				            #( 1 3 3 'Missing opener for closer: ]' )
				            #( 4 5 5 'Missing opener for closer: )' ) )).
		        (self new
			         source: '(1}2)';
			         nodeAt: '23154';
			         formattedCode: '( 1 }. 2 )';
			         notices:
				         #( #( 1 2 3 ''')'' expected' )
				            #( 1 3 3 'Missing opener for closer: }' )
				            #( 4 5 5 'Missing opener for closer: )' ) )).
		        (self new
			         source: '(1. 2)';
			         nodeAt: '120043';
			         formattedCode: '( 1. 2 )';
			         notices:
				         #( #( 1 2 3 ''')'' expected' )
				            #( 5 6 6 'Missing opener for closer: )' ) )).
		        (self new
			         source: '[1)2]';
			         nodeAt: '03240';
			         formattedCode: '[ 1 ). 2 ]';
			         notices: #( #( 2 3 3 'Missing opener for closer: )' ) )).
		        (self new
			         source: '[1}2]';
			         nodeAt: '03240';
			         formattedCode: '[ 1 }. 2 ]';
			         notices: #( #( 2 3 3 'Missing opener for closer: }' ) )).
		        (self new
			         source: '#(1]2}3)';
			         nodeAt: '00123450';
			         formattedCode: '#( 1 #'']'' 2 #''}'' 3 )';
			         isFaulty: false;
			         value: #( 1 #']' 2 #'}' 3 )). "`#(` can eat almost anything"
		        (self new
			         source: '#( 0 1r2 4 )';
			         nodeAt: '000102330400';
			         formattedCode: '#( 0 1 r2 4 )';
			         notices:
				         #( #( 6 6 7 'an integer greater than 1 as valid radix expected' ) )). "Almost anything..."
		        (self new
			         source: '#[ 1 ) 2 ]';
			         nodeAt: '0001020300';
			         notices: #( #( 6 6 6 '8-bit integer expected' ) )).
		        (self new
			         source: '#[ 1 } 2 ]';
			         nodeAt: '0001020300';
			         notices: #( #( 6 6 6 '8-bit integer expected' ) )).
		        (self new
			         source: '#[ 1 a 2 ]';
			         nodeAt: '0001020300';
			         notices: #( #( 6 6 6 '8-bit integer expected' ) )).
		        (self new
			         source: '#[ 1 -1 2 ]';
			         nodeAt: '00010220300';
			         notices: #( #( 6 7 6 '8-bit integer expected' ) )).
		        (self new
			         source: '#[ 1 1.0 2 ]';
			         nodeAt: '000102220300';
			         notices: #( #( 6 8 6 '8-bit integer expected' ) )).
		        (self new
			         source: '#[ 1 256 2 ]';
			         nodeAt: '000102220300';
			         notices: #( #( 6 8 6 '8-bit integer expected' ) )).
		        (self new
			         source: '{1)2}';
			         nodeAt: '02130';
			         formattedCode: '{ 1 ). 2 }';
			         notices: #( #( 2 3 3 'Missing opener for closer: )' ) )).
		        (self new
			         source: '{1]2}';
			         nodeAt: '02130';
			         formattedCode: '{ 1 ]. 2 }';
			         notices: #( #( 2 3 3 'Missing opener for closer: ]' ) )).

		        "...or without expected thing"
		        "Note: all compounds `[]` `#()` `#[]` `{}` are legal empty, except one"
		        (self new
			         source: '()';
			         nodeAt: '00';
			         notices: #( #( 1 2 2 'Variable or expression expected' ) )).
		        (self new
			         source: '[ ]';
			         nodeAt: '000';
			         isFaulty: false).
		        (self new
			         source: '#( )';
			         nodeAt: '0000';
			         isFaulty: false;
			         value: #(  )).
		        (self new
			         source: '#[ ]';
			         nodeAt: '0000';
			         isFaulty: false;
			         value: #[  ]).
		        (self new
			         source: '{ }';
			         nodeAt: '000';
			         isFaulty: false;
			         value: #(  )).

		        "Bad sequence. The first expression is considered unfinished."
		        (self new
			         source: '1 2';
			         nodeAt: '213';
			         formattedCode: '1. 2';
			         notices: #( #( 1 2 3 'End of statement expected' ) )).
		        (self new
			         source: '1 foo 2';
			         nodeAt: '3222214';
			         formattedCode: '1 foo. 2';
			         notices: #( #( 1 6 7 'End of statement expected' ) )).
		        (self new
			         source: '(1)2';
			         nodeAt: '2223';
			         formattedCode: '1. 2';
			         notices: #( #( 1 3 4 'End of statement expected' ) )).
		        (self new
			         source: '1(2)';
			         nodeAt: '2333';
			         formattedCode: '1. 2';
			         notices: #( #( 1 1 2 'End of statement expected' ) )).
		        (self new
			         source: '(1)(2)';
			         nodeAt: '222333';
			         formattedCode: '1. 2';
			         notices: #( #( 1 3 4 'End of statement expected' ) )).
		        (self new
			         source: '#hello#world';
			         nodeAt: '222222333333';
			         formattedCode: '#hello. #world';
			         notices: #( #( 1 6 7 'End of statement expected' ) )).
		        (self new
			         source: '$h$w';
			         nodeAt: '2233';
			         formattedCode: '$h. $w';
			         notices: #( #( 1 2 3 'End of statement expected' ) )).
		        (self new
			         source: '[1][2]';
			         nodeAt: '242575';
			         formattedCode: '[ 1 ]. [ 2 ]';
			         notices: #( #( 1 3 4 'End of statement expected' ) )).
		        (self new
			         source: '{1}{2}';
			         nodeAt: '232454';
			         formattedCode: '{ 1 }. { 2 }';
			         notices: #( #( 1 3 4 'End of statement expected' ) )).
		        (self new
			         source: '#(1)#(2)';
			         nodeAt: '22324454';
			         formattedCode: '#( 1 ). #( 2 )';
			         notices: #( #( 1 4 5 'End of statement expected' ) )).
		        (self new
			         source: '#[1]#[2]';
			         nodeAt: '22324454';
			         formattedCode: '#[ 1 ]. #[ 2 ]';
			         notices: #( #( 1 4 5 'End of statement expected' ) )).

		        "Bad temporary variable declarations"
		        "Note: bad temporaries will be stored as error statements"
		        (self new
			         source: '| ';
			         nodeAt: '0';
			         formattedCode: '| | ';
			         notices: #( #( 1 1 3 '''|'' or variable expected' ) )).
		        (self new
			         source: '| a b';
			         nodeAt: '00102';
			         formattedCode: '| a b | ';
			         notices: #( #( 1 5 6 '''|'' or variable expected' ) )).
		        (self new
			         source: '|- 1';
			         nodeAt: '1224';
			         formattedCode: '| | . - 1';
			         notices: #( #( 1 1 2 '''|'' or variable expected' )
				            #( 2 1 2 'Variable or expression expected' ) )). "Here |- is scanned as a single operator token, so parser must work more"
		        (self new
			         source: '| 1';
			         nodeAt: '102';
			         formattedCode: '| | . 1';
			         notices: #( #( 1 1 3 '''|'' or variable expected' ) )).
		        "Note that the | character is also a binary operator, so a missing opening | become a binary call with a missing argument (see bellow)"
		        (self new
			         source: 'a | ';
			         nodeAt: '1000';
			         notices:
				         #( #( 1 1 1 'Undeclared variable' )
				            #( 5 4 5 'Variable or expression expected' ) )).
		        (self new
			         source: 'a || ';
			         nodeAt: '10000';
			         notices:
				         #( #( 1 1 1 'Undeclared variable' )
				            #( 6 5 6 'Variable or expression expected' ) )).
		        (self new
			         source: '| | a';
			         nodeAt: '    0';
			         formattedCode: 'a';
			         isParseFaulty: false;
			         isFaultyMinusUndeclared: false;
			         raise: UndeclaredVariableRead;
			         notices: #( #( 5 5 5 'Undeclared variable' ) )). "This one is legal, it is an empty list of temporaries, the | are dismissed"
		        (self new
			         source: '|| a';
			         nodeAt: '   0';
			         formattedCode: 'a';
			         isParseFaulty: false;
			         isFaultyMinusUndeclared: false;
			         raise: UndeclaredVariableRead;
			         notices: #( #( 4 4 4 'Undeclared variable' ) )). "Same, but are messing with the Scanner"
		        (self new
			         source: ' ||| a';
			         nodeAt: '   002';
			         formattedCode: ' | a';
			         notices: #( #( 4 3 4 'Variable or expression expected' )
				            #( 6 6 6 'Undeclared variable' ) )). "this one is a empty temps and a binary operator | with a mising receiver"
		        (self new
			         source: ' |||| a';
			         nodeAt: '   0002';
			         formattedCode: ' || a';
			         notices: #( #( 4 3 4 'Variable or expression expected' )
				            #( 7 7 7 'Undeclared variable' ) )). "this one is a empty temps and a binary operator || with a mising receiver"
		        (self new
			         source: '| a | | a';
			         nodeAt: '001000224';
			         notices: #( #( 7 6 7 'Variable or expression expected' )
				            #( 9 9 9 'Unitialized variable' ) )). "A valid temporary followed by a binary operator with a missing receiver"
		        (self new
			         source: '| a ||a';
			         nodeAt: '0010024';
			         formattedCode: '| a | | a';
			         notices: #( #( 6 5 6 'Variable or expression expected' )
				            #( 7 7 7 'Unitialized variable' ) )). "same"

		        "Unexpected parameters (or columns)"
		        "Note that `:a` is not a token but a special `:` followed by an identifier, whereas `a:` is a single token."
		        "Nevertheless, the parser will try to catch unexpected :a together"
		        (self new
			         source: ':a';
			         nodeAt: '00';
			         notices: #( #( 1 2 1 'Unexpected block parameter' ) )).
		        (self new
			         source: '::a';
			         nodeAt: '122';
			         formattedCode: ':. :a';
			         notices:
				         #( #( 1 1 1 'Unexpected token' )
				            #( 2 3 2 'Unexpected block parameter' ) )).
		        (self new
			         source: ':::a';
			         nodeAt: '1233';
			         formattedCode: ':. :. :a';
			         notices:
				         #( #( 1 1 1 'Unexpected token' )
				            #( 2 2 2 'Unexpected token' )
				            #( 3 4 3 'Unexpected block parameter' ) )).
		        (self new
			         source: '::';
			         nodeAt: '12';
			         formattedCode: ':. :';
			         notices:
				         #( #( 1 1 1 'Unexpected token' )
				            #( 2 2 2 'Unexpected token' ) )).
		        (self new
			         source: ':a foo';
			         nodeAt: '110000';
			         notices: #( #( 1 2 1 'Unexpected block parameter' ) )).
		        (self new
			         source: 'a :foo';
			         nodeAt: '213333';
			         formattedCode: 'a. :foo';
			         notices:
				         #( #( 1 1 1 'Undeclared variable' )
				            #( 1 2 3 'End of statement expected' )
				            #( 3 6 3 'Unexpected block parameter' ) )).
		        (self new
			         source: 'a : foo';
			         nodeAt: '2133333';
			         formattedCode: 'a. :foo';
			         notices:
				         #( #( 1 1 1 'Undeclared variable' )
				            #( 1 2 3 'End of statement expected' )
				            #( 3 7 3 'Unexpected block parameter' ) )).
		        (self new
			         source: 'a:';
			         nodeAt: '00';
			         formattedCode: ' a: ';
			         notices: #( #( 1 0 1 'Variable or expression expected' )
				            #( 3 2 3 'Variable or expression expected' ) )). "keyword message with a missing receiver and argument"
		        (self new
			         source: 'a::';
			         nodeAt: '000';
			         notices: #( #( 1 3 1 'Unexpected token' ) )). "just a bad token"
		        (self new
			         source: 'a:foo';
			         nodeAt: '00222';
			         formattedCode: ' a: foo';
			         notices: #( #( 1 0 1 'Variable or expression expected' )
				            #( 3 5 3 'Undeclared variable' ) )). "keyword message with a missing receiver"
		        (self new
			         source: 'a::foo';
			         nodeAt: '111222';
			         formattedCode: 'a::. foo';
			         notices:
				         #( #( 1 3 1 'Unexpected token' )
				            #( 4 6 4 'Undeclared variable' ) )).
		        (self new
			         source: ':a:foo';
			         nodeAt: '122444';
			         formattedCode: ':. a: foo';
			         notices:
				         #( #( 1 1 1 'Unexpected token' )
				            #( 2 1 2 'Variable or expression expected' )
				            #( 4 6 4 'Undeclared variable' ) )).
		        (self new
			         source: '|:a|';
			         nodeAt: '1332';
			         formattedCode: '| | . :a | ';
			         notices: #( #( 1 1 2 '''|'' or variable expected' )
				            #( 2 3 2 'Unexpected block parameter' )
				            #( 5 4 5 'Variable or expression expected' ) )).
		        (self new
			         source: '|:a';
			         nodeAt: '122';
			         formattedCode: '| | . :a';
			         notices: #( #( 1 1 2 '''|'' or variable expected' )
				            #( 2 3 2 'Unexpected block parameter' ) )).
		        (self new
			         source: '|::a';
			         nodeAt: '1233';
			         formattedCode: '| | . :. :a';
			         notices: #( #( 1 1 2 '''|'' or variable expected' )
				            #( 2 2 2 'Unexpected token' )
				            #( 3 4 3 'Unexpected block parameter' ) )).
		        (self new
			         source: '|a:|';
			         nodeAt: '1224';
			         formattedCode: '| | . a: | ';
			         notices: #( #( 1 1 2 '''|'' or variable expected' )
				            #( 2 1 2 'Variable or expression expected' )
				            #( 4 3 4 'Variable or expression expected' )
				            #( 5 4 5 'Variable or expression expected' ) )).
		        (self new
			         source: '|a:';
			         nodeAt: '122';
			         formattedCode: '| | . a: ';
			         notices: #( #( 1 1 2 '''|'' or variable expected' )
				            #( 2 1 2 'Variable or expression expected' )
				            #( 4 3 4 'Variable or expression expected' ) )).

		        "Bad block parameters"
		        "A bad parameter cause a error object to be added as the last element of the block parameter.
		On formating, a double space can be seen."
		        (self new
			         source: '[:a b]';
			         nodeAt: '001040';
			         formattedCode: '[ :a | b ]';
			         raise: UndeclaredVariableRead;
			         notices: #( #( 5 4 5 '''|'' or parameter expected' )
				            #( 5 5 5 'Undeclared variable' ) )). "FIXME"
		        (self new
			         source: '[:a 1]';
			         nodeAt: '001040';
			         formattedCode: '[ :a | 1 ]';
			         value: 1;
			         notices: #( #( 5 4 5 '''|'' or parameter expected' ) )). "FIXME"
		        (self new
			         source: '[:a :]';
			         nodeAt: '001000';
			         formattedCode: '[ :a : | ]';
			         value: nil;
			         notices: #( #( 6 5 6 'Variable name expected' ) )). "FIXME"
		        (self new
			         source: '[:a ::b]';
			         nodeAt: '00100220';
			         formattedCode: '[ :a ::b | ]';
			         value: nil;
			         notices: #( #( 6 7 6 'Variable name expected' ) )). "FIXME"
		        (self new
			         source: '[:a :b]';
			         nodeAt: '0010020';
			         formattedCode: '[ :a :b | ]';
			         isFaulty: false). "no pipe (and no body) is legal"
		        (self new
			         source: '[: a : b]';
			         nodeAt: '000100020';
			         formattedCode: '[ :a :b | ]';
			         isFaulty: false). "spaces are also legal"
		        (self new
			         source: '[:a:b]';
			         nodeAt: '004460';
			         formattedCode: '[ : | a: b ]';
			         notices: #( #( 3 2 3 'Variable name expected' )
				            #( 3 2 3 '''|'' or parameter expected' )
				            #( 3 2 3 'Name already defined' )
				            #( 3 2 3 'Variable or expression expected' )
				            #( 5 5 5 'Undeclared variable' ) )). "FIXME?"
		        (self new
			         source: '[ a: ]';
			         nodeAt: '002220';
			         notices: #( #( 3 2 3 'Variable or expression expected' )
				            #( 6 5 6 'Variable or expression expected' ) )). "no parameters, a keyword message send witout receiver nor arguments"
		        (self new
			         source: '[ | ]';
			         nodeAt: '00200';
			         formattedCode: '[ | | ]';
			         notices: #( #( 3 3 5 '''|'' or variable expected' ) )).
		        (self new
			         source: '[ | b ]';
			         nodeAt: '0022300';
			         formattedCode: '[ | b | ]';
			         notices: #( #( 3 5 7 '''|'' or variable expected' ) )).
		        (self new
			         source: '[ :a | | a b ]';
			         nodeAt: '00010003343500';
			         formattedCode: '[ :a | | a b | ]';
			         notices: #( #( 8 12 14 '''|'' or variable expected' ) )).
		        (self new
			         source: '[ :a || a b ]';
			         nodeAt: '0001003343500';
			         formattedCode: '[ :a | | a b | ]';
			         notices: #( #( 7 11 13 '''|'' or variable expected' ) )).
		        (self new
			         source: '[:a| | |b]';
			         nodeAt: '0010022230';
			         formattedCode: '[ :a | b ]';
			         isParseFaulty: false;
			         isFaultyMinusUndeclared: false;
			         raise: UndeclaredVariableRead;
			         notices: #( #( 9 9 9 'Undeclared variable' ) )). "Explicit empty list of temporaries"
		        (self new
			         source: '[:a| ||a]';
			         nodeAt: '001002230';
			         formattedCode: '[ :a | a ]';
			         isFaulty: false;
			         value: 1). "Same but mess with the Scanner"
		        (self new
			         source: '[:a|| |a]';
			         nodeAt: '001022230';
			         formattedCode: '[ :a | a ]';
			         isFaulty: false;
			         value: 1). "Same"
		        (self new
			         source: '[:a|||a]';
			         nodeAt: '00102230';
			         formattedCode: '[ :a | a ]';
			         isFaulty: false;
			         value: 1). "Same"
		        (self new
			         source: '[:a||||a]';
			         nodeAt: '001022350';
			         formattedCode: '[ :a | | a ]';
			         notices: #( #( 7 6 7 'Variable or expression expected' ) )). "same + binary | without receiver"

		        "Unclosed blocks"
		        (self new
			         source: '[ : | ';
			         nodeAt: '000000';
			         notices: #( #( 1 6 7 ''']'' expected' ) )).
		        (self new
			         source: '[:';
			         nodeAt: '00';
			         formattedCode: '[ : | ';
			         notices: #( #( 1 2 3 ''']'' expected' ) )).
		        (self new
			         source: '[ :a :b | ';
			         nodeAt: '0000000000';
			         notices: #( #( 1 10 11 ''']'' expected' ) )).
		        (self new
			         source: '[ :a :b';
			         nodeAt: '0000000';
			         formattedCode: '[ :a :b | ';
			         notices: #( #( 1 7 8 ''']'' expected' ) )).
		        (self new
			         source: '[ :a a';
			         nodeAt: '000002';
			         formattedCode: '[ :a | a';
			         notices: #( #( 1 6 7 ''']'' expected' ) )).
		        (self new
			         source: '[ :a b';
			         nodeAt: '000002';
			         formattedCode: '[ :a | b';
			         notices:
				         #( #( 6 6 6 'Undeclared variable' )
				            #( 1 6 7 ''']'' expected' ) )).
		        (self new
			         source: '[ :a | ';
			         nodeAt: '0000000';
			         notices: #( #( 1 7 8 ''']'' expected' ) )).
		        (self new
			         source: '[ :a | a';
			         nodeAt: '00000002';
			         notices: #( #( 1 8 9 ''']'' expected' ) )).
		        (self new
			         source: '[ :a | b';
			         nodeAt: '00000002';
			         notices:
				         #( #( 8 8 8 'Undeclared variable' )
				            #( 1 8 9 ''']'' expected' ) )).
		        (self new
			         source: '[ | ';
			         nodeAt: '0020';
			         formattedCode: '[ | | ';
			         notices: #( #( 3 3 5 '''|'' or variable expected' )
				            #( 1 4 5 ''']'' expected' ) )).
		        (self new
			         source: '[ | 1';
			         nodeAt: '00213';
			         formattedCode: '[ | | . 1';
			         notices: #( #( 3 3 5 '''|'' or variable expected' )
				            #( 1 5 6 ''']'' expected' ) )).
		        (self new
			         source: '[ | a b';
			         nodeAt: '0022324';
			         formattedCode: '[ | a b | ';
			         notices: #( #( 3 7 8 '''|'' or variable expected' )
				            #( 1 7 8 ''']'' expected' ) )) }.

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

	<script: 'self styleWithError: self badMethods'>
	| list |
	list := {
		        (self new
			         source: ' ';
			         nodeAt: '0';
			         notices: #( #( 2 1 2 'Message pattern expected' ) )).

		        "Wrong token for pattern"
		        "An empty pattern will be set, and the first statement will be a error node."
		        (self new
			         source: '5';
			         nodeAt: '3';
			         formattedCode: ' . 5';
			         notices: #( #( 1 0 1 'Message pattern expected' ) )).
		        (self new
			         source: '''hello''';
			         nodeAt: '3333333';
			         formattedCode: ' . ''hello''';
			         notices: #( #( 1 0 1 'Message pattern expected' ) )).
		        (self new
			         source: '#hello';
			         nodeAt: '333333';
			         formattedCode: ' . #hello';
			         notices: #( #( 1 0 1 'Message pattern expected' ) )).
		        (self new
			         source: ':';
			         nodeAt: '3';
			         formattedCode: ' . :';
			         notices: #( #( 1 0 1 'Message pattern expected' )
				            #( 1 1 1 'Unexpected token' ) )).
		        (self new
			         source: '#(foo bar)';
			         nodeAt: '3344435553';
			         formattedCode: ' . #( foo bar )';
			         notices: #( #( 1 0 1 'Message pattern expected' ) )).

		        "Random bad token"
		        "Tought: is complainng about the bad token really better than complaining about the bad pattern?"
		        (self new
			         source: ' $';
			         nodeAt: '02';
			         notices: #( #( 2 2 3 'Character expected' ) )).
		        (self new
			         source: ' ''hello';
			         nodeAt: '0222222';
			         notices: #( #( 2 7 8 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: ' 2r3';
			         nodeAt: '0223';
			         formattedCode: ' 2r. 3';
			         notices:
				         #( #( 2 3 4 'a digit between 0 and 1 expected' ) )).

		        "Bad aruments"
		        "The missing argument will be an error, the remainer starts the body"
		        (self new
			         source: '+ ';
			         nodeAt: '00';
			         value: nil;
			         notices: #( #( 3 2 3 'Variable name expected' ) )).
		        (self new
			         source: '+ 1';
			         nodeAt: '003';
			         value: nil;
			         notices: #( #( 3 2 3 'Variable name expected' ) )).
		        (self new
			         source: '+ foo: ';
			         nodeAt: '0033333';
			         notices: #( #( 3 2 3 'Variable name expected' )
				            #( 3 2 3 'Variable or expression expected' )
				            #( 8 7 8 'Variable or expression expected' ) )).
		        (self new
			         source: 'foo: ';
			         nodeAt: '00000';
			         value: nil;
			         notices: #( #( 6 5 6 'Variable name expected' ) )).
		        (self new
			         source: 'foo: 1';
			         nodeAt: '000003';
			         value: nil;
			         notices: #( #( 6 5 6 'Variable name expected' ) )).
		        (self new
			         source: 'foo: + ';
			         nodeAt: '0000033';
			         notices: #( #( 6 5 6 'Variable name expected' )
				            #( 6 5 6 'Variable or expression expected' )
				            #( 8 7 8 'Variable or expression expected' ) )).
		        (self new
			         source: 'foo: bar: ';
			         nodeAt: '0000000000';
			         value: nil;
			         notices: #( #( 6 5 6 'Variable name expected' )
				            #( 11 10 11 'Variable name expected' )
				            #( 11 10 11 'Name already defined' ) )).
		        (self new
			         source: 'foo:bar:';
			         nodeAt: '33333333';
			         formattedCode: ' . foo:bar:';
			         notices: #( #( 1 0 1 'Message pattern expected' )
				            #( 1 8 1 'Unexpected token' ) )). "`foo:bar:` is a single token, and is unexpected"

		        "Bad pragma message"
		        (self new
			         source: 'foo < ';
			         nodeAt: '000011';
			         value: nil;
			         notices: #( #( 7 6 7 'Message pattern expected' )
				            #( 5 6 7 '''>'' expected' ) )).
		        (self new
			         source: 'foo <> ';
			         nodeAt: '0000222';
			         notices: #( #( 5 4 5 'Variable or expression expected' )
				            #( 8 7 8 'Variable or expression expected' ) )).
		        (self new
			         source: 'foo < 4';
			         nodeAt: '0000114';
			         value: nil;
			         notices: #( #( 7 6 7 'Message pattern expected' )
				            #( 5 6 7 '''>'' expected' ) )).
		        (self new
			         source: 'foo < bar ';
			         nodeAt: '0000222222';
			         value: nil;
			         notices: #( #( 5 10 11 '''>'' expected' ) )).
		        (self new
			         source: 'foo < bar: ';
			         nodeAt: '00002222222';
			         value: nil;
			         notices: #( #( 12 11 12 'Literal constant expected' )
				            #( 5 11 12 '''>'' expected' ) )).
		        (self new
			         source: 'foo < bar: 1 1 > ';
			         nodeAt: '00002222222326555';
			         notices:
				         #( #( 5 13 14 '''>'' expected' )
				            #( 18 17 18 'Variable or expression expected' ) )).
		        (self new
			         source: 'foo < bar ; baz > ';
			         nodeAt: '0000222222888884AA';
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
			         nodeAt: '000011111110';
			         value: nil;
			         notices: #( #( 11 10 11 'Literal constant expected' ) )).
		        (self new
			         source: 'foo <bar:(1)>';
			         nodeAt: '0000222226665';
			         formattedCode: 'foo < bar: 1 > ';
			         notices: #( #( 10 9 10 'Literal constant expected' )
				            #( 5 9 10 '''>'' expected' )
				            #( 14 13 14 'Variable or expression expected' ) )). "FIXME. dont eat parentheses"
		        (self new
			         source: 'foo < bar: baz > ';
			         nodeAt: '00002222222666555';
			         notices: #( #( 12 11 12 'Literal constant expected' )
				            #( 5 11 12 '''>'' expected' )
				            #( 12 14 12 'Undeclared variable' )
				            #( 18 17 18 'Variable or expression expected' ) )).
		        (self new
			         source: 'foo < bar: 1 + 1 > ';
			         nodeAt: '0000222222232668555';
			         notices:
				         #( #( 5 13 14 '''>'' expected' )
				            #( 14 13 14 'Variable or expression expected' )
				            #( 20 19 20 'Variable or expression expected' ) )).
		        (self new
			         source: 'foo < bar: [ 1 ] > ';
			         nodeAt: '0000222222266866555';
			         notices: #( #( 12 11 12 'Literal constant expected' )
				            #( 5 11 12 '''>'' expected' )
				            #( 20 19 20 'Variable or expression expected' ) )).
		        (self new
			         source: 'foo < bar: { 1 } > ';
			         nodeAt: '0000222222266766555';
			         notices: #( #( 12 11 12 'Literal constant expected' )
				            #( 5 11 12 '''>'' expected' )
				            #( 20 19 20 'Variable or expression expected' ) )).
		        (self new
			         source: 'foo <bar: #[ -1 ]> ';
			         nodeAt: '0000111111222332210';
			         value: nil;
			         notices: #( #( 14 15 14 '8-bit integer expected' ) )). "Literal bytes arrays are acceptable, but this one is faulty"
		        (self new
			         source: 'foo < + 1> ';
			         nodeAt: '00001111210';
			         isFaulty: false). "Binary message is legal pragma"
		        (self new
			         source: 'foo < + > ';
			         nodeAt: '0000111110';
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
RBCodeSnippet class >> badPositions [
	"This list focuses on node positions (and later highlighing)"

	<script:
	'self styleWithError: self badPositions. self updateSnippets: self badPositions'>
	| list |
	list := {
		        (self new
			         source: ' foo | tmp | tmp := 1 . ^ tmp . ';
			         nodeAt: '00000112221115553333411166777110';
			         formattedCode: 'foo | tmp | tmp := 1. ^ tmp';
			         isMethod: true;
			         value: 1).
		        (self new
			         source: ' foo: arg bar: arr ^ arg + arr . ';
			         nodeAt: '000000111000000222044666555777330';
			         formattedCode: 'foo: arg bar: arr ^ arg + arr';
			         isMethod: true;
			         value: 3).
		        (self new
			         source: ' | tmp | tmp := 1 . ^ tmp . ';
			         nodeAt: ' 00111000444222230005566600';
			         formattedCode: '| tmp | tmp := 1. ^ tmp';
			         value: 1).
		        (self new
			         source: ' foo: arg ^ arg min: arg + 2 ; abs . ';
			         nodeAt: '000000111033BBB5555558887779AAAAAA220';
			         formattedCode: 'foo: arg ^ arg min: arg + 2; abs';
			         isMethod: true;
			         value: 1).
		        (self new
			         source: ' foo: arg ^ ( ( ( ( arg ) ) + ( ( 1 ) ) ) ) . ';
			         nodeAt: '0000001110334444555555555554446666666664444220';
			         formattedCode: 'foo: arg ^ arg + 1';
			         isMethod: true;
			         value: 2).
		        (self new
			         source: ' ( [ :aaa : bbb | | ccc ddd | aaa . ] ) . ';
			         nodeAt: ' 00000111000222000334443555333666330000';
			         formattedCode: '[ :aaa :bbb | | ccc ddd | aaa ]';
			         value: 1;
			         notices:
				         #( #( 21 23 21 'Unused variable' )
				            #( 25 27 25 'Unused variable' ) )) }.

	"Setup default values"
	self new
		group: #badPositions;
		isMethod: false;
		isFaulty: false;
		applyDefaultTo: list.
	^ list
]

{ #category : #accessing }
RBCodeSnippet class >> badSemantic [
	"List of varions semantic and backend errors."

	<script: 'self styleWithError: self badSemantic'>
	| list |
	"Undefined variable
	For some reason, this is just a warning in non-faulty mode, not an error."
	list := {
		        (self new
			         source: 'a := 10. ^ a';
			         nodeAt: '311112200445';
			         isFaulty: true;
			         isFaultyMinusUndeclared: false;
			         raise: UndeclaredVariableWrite;
			         notices:
				         #( #( 1 1 1 'Undeclared variable' )
				            #( 12 12 12 'Undeclared variable' ) )).
		        (self new
			         source: '^ a';
			         nodeAt: '001';
			         isFaulty: true;
			         isFaultyMinusUndeclared: false;
			         raise: UndeclaredVariableRead;
			         notices: #( #( 3 3 3 'Undeclared variable' ) )).

		        "Uninitialized variable"
		        (self new
			         source: '| a | ^ a';
			         nodeAt: '001000223';
			         notices: #( #( 9 9 9 'Unitialized variable' ) )).
		        (self new
			         source: '| a | [ a := 10 ]. ^ a';
			         nodeAt: '0010002264444552200778').
		        (self new
			         source: '| a | [ ^ a ]. a := 10';
			         nodeAt: '0010002244522008666677';
			         value: 10;
			         notices: #( #( 11 11 11 'Unitialized variable' ) )).

		        "Duplicated variable definition (same scope)"
		        (self new
			         source: 'foo: a bar: a ^ a';
			         nodeAt: '00000100000020445';
			         isMethod: true;
			         value: 1;
			         notices: #( #( 13 13 13 'Name already defined' ) )).
		        (self new
			         source: '| a a | a := 10. ^ a';
			         nodeAt: '00102000533334400667';
			         value: 10;
			         notices:
				         #( #( 3 3 3 'Unused variable' )
				            #( 5 5 5 'Name already defined' ) )).
		        (self new
			         source: '[ | a a | a := 10. a ]';
			         nodeAt: '0011213111644445511700';
			         value: 10;
			         notices:
				         #( #( 5 5 5 'Unused variable' )
				            #( 7 7 7 'Name already defined' ) )).
		        (self new
			         source: '[ :a :a | a ]';
			         nodeAt: '0001002000400';
			         value: 1;
			         notices: #( #( 7 7 7 'Name already defined' ) )).

		        "Shadowed variables"
		        (self new
			         source: 'foo: a ^ [ | a | a := 10. a ] value + a';
			         nodeAt: '00000103366778777B9999AA77C66555555444D';
			         isMethod: true;
			         value: 11;
			         notices: #( #( 14 14 14 'Name already defined' ) )).
		        (self new
			         source:
				         'foo | a | a := 1. ^ [ | a | a := 10. a ] value + a';
			         nodeAt:
				         '0000112111533334116699AABAAAECCCCDDAAF99888888777G';
			         isMethod: true;
			         value: 11;
			         notices: #( #( 25 25 25 'Name already defined' ) )).
		        (self new
			         source:
				         'foo ^ [ | a | a := 1. [ | a | a := 10. a ] value + a ] value';
			         nodeAt:
				         '0000224455655597777855CCDDEDDDHFFFFGGDDICCBBBBBBAAAJ44333333';
			         isMethod: true;
			         value: 11;
			         notices: #( #( 27 27 27 'Name already defined' ) )).
		        (self new
			         source:
				         'foo ^ [ :a | [ | a | a := 10. a ] value + a ] value: 1';
			         nodeAt:
				         '000022444544499AABAAAECCCCDDAAF99888888777G4433333333H';
			         isMethod: true;
			         value: 11;
			         notices: #( #( 18 18 18 'Name already defined' ) )).
		        (self new
			         source: 'foo: a ^ [ :a | a ] value: 10 + a';
			         nodeAt: '000001033555655585544444444AA999B';
			         isMethod: true;
			         value: 11;
			         notices: #( #( 13 13 13 'Name already defined' ) )).
		        (self new
			         source: 'foo | a | a := 1. ^ [ :a | a ] value: 10 + a';
			         nodeAt: '000011211153333411668889888B8877777777DDCCCE';
			         isMethod: true;
			         value: 11;
			         notices: #( #( 24 24 24 'Name already defined' ) )).
		        (self new
			         source:
				         'foo ^ [ | a | a := 1. [ :a | a ] value: 10 + a ] value';
			         nodeAt:
				         '0000224455655597777855BBBCBBBEBBAAAAAAAAGGFFFH44333333';
			         isMethod: true;
			         value: 11;
			         notices: #( #( 26 26 26 'Name already defined' ) )).
		        (self new
			         source: 'foo ^ [ :a | [ :a | a ] value: 10 + a ] value: 1';
			         nodeAt: '00002244454448889888B8877777777DDCCCE4433333333F';
			         isMethod: true;
			         value: 11;
			         notices: #( #( 17 17 17 'Name already defined' ) )). "phonyArgs"

		        "Write on readonly or reserved"
		        (self new
			         source: 'foo: a a := 10. ^ a';
			         nodeAt: '0000010533334422667';
			         isMethod: true;
			         isFaulty: true;
			         notices:
				         #( #( 8 8 8 'Assignment to read-only variable' ) )).
		        (self new
			         source: '[ :a | a := 10. a ]';
			         nodeAt: '0001000533334422600';
			         isFaulty: true;
			         notices:
				         #( #( 8 8 8 'Assignment to read-only variable' ) )).
		        "The following assignment are explicitely no-op, to minimize the impact if, for some broken reasons, they are really executed"
		        (self new
			         source: 'nil := nil';
			         nodeAt: '2221333444';
			         formattedCode: 'nil. := nil';
			         isFaulty: true;
			         isParseFaulty: true;
			         notices: #( #( 1 4 5 'End of statement expected' )
				            #( 5 10 5 'variable expected in assigment' ) )). "it is a literal, not an identifier"
		        (self new
			         source: 'true := true';
			         nodeAt: '222213334444';
			         formattedCode: 'true. := true';
			         isFaulty: true;
			         isParseFaulty: true;
			         notices: #( #( 1 5 6 'End of statement expected' )
				            #( 6 12 6 'variable expected in assigment' ) )). "it is a literal, not an identifier"
		        (self new
			         source: 'false := false';
			         nodeAt: '22222133344444';
			         formattedCode: 'false. := false';
			         isFaulty: true;
			         isParseFaulty: true;
			         notices: #( #( 1 6 7 'End of statement expected' )
				            #( 7 14 7 'variable expected in assigment' ) )). "it is a literal, not an identifier"
		        (self new
			         source: 'self := self';
			         nodeAt: '222200001111';
			         isFaulty: true;
			         numberOfCritiques: 1;
			         notices: #( #( 1 4 1 'Assigment to reserved variable' )
				            #( 1 4 1 'Assignment to read-only variable' ) )).
		        (self new
			         source: 'super := super';
			         nodeAt: '22222000011111';
			         isFaulty: true;
			         numberOfCritiques: 1;
			         notices: #( #( 1 5 1 'Assigment to reserved variable' )
				            #( 1 5 1 'Assignment to read-only variable' ) )).
		        (self new
			         source: 'thisContext := thisContext';
			         nodeAt: '22222222222000011111111111';
			         isFaulty: true;
			         numberOfCritiques: 1;
			         notices: #( #( 1 11 1 'Assigment to reserved variable' )
				            #( 1 11 1 'Assignment to read-only variable' ) )).
		        (self new
			         source: 'Object := Object';
			         nodeAt: '2222220000111111';
			         isFaulty: true;
			         numberOfCritiques: 1;
			         notices:
				         #( #( 1 6 1 'Assignment to read-only variable' ) )).

		        "Shadowed reserved or global"
		        (self new
			         source: '| self | self := 1. ^ self';
			         nodeAt: '00111100044442222300556666';
			         isFaulty: true;
			         value: 1;
			         notices: #( #( 3 6 3 'Reserved variable name' ) )).
		        (self new
			         source: '| super | super := 1. ^ super';
			         nodeAt: '00111110004444422223005566666';
			         isFaulty: true;
			         value: 1;
			         notices: #( #( 3 7 3 'Reserved variable name' ) )).
		        (self new
			         source: '| thisContext | thisContext := 1. ^ thisContext';
			         nodeAt: '00111111111110004444444444422223005566666666666';
			         isFaulty: true;
			         value: 1;
			         notices: #( #( 3 13 3 'Reserved variable name' ) )).
		        (self new
			         source: '| Object | Object := 1. ^ Object';
			         nodeAt: '00111111000444444222230055666666';
			         value: 1;
			         notices: #( #( 3 8 3 'Name already defined' ) )).
		        (self new
			         source: 'foo: self ^ self + 1';
			         nodeAt: '00000111103355554446';
			         isMethod: true;
			         isFaulty: true;
			         value: 2;
			         notices: #( #( 6 9 6 'Reserved variable name' ) )).
		        (self new
			         source: 'foo: super ^ super + 1';
			         nodeAt: '0000011111033555554446';
			         isMethod: true;
			         isFaulty: true;
			         value: 2;
			         notices: #( #( 6 10 6 'Reserved variable name' ) )).
		        (self new
			         source: 'foo: thisContext ^ thisContext + 1';
			         nodeAt: '0000011111111111033555555555554446';
			         isMethod: true;
			         isFaulty: true;
			         value: 2;
			         notices: #( #( 6 16 6 'Reserved variable name' ) )).
		        (self new
			         source: 'foo: Object ^ Object + 1';
			         nodeAt: '000001111110335555554446';
			         isMethod: true;
			         value: 2;
			         notices: #( #( 6 11 6 'Name already defined' ) )).
		        (self new
			         source: '[ :self | self + 1 ]';
			         nodeAt: '00011110004444333500';
			         isFaulty: true;
			         value: 2;
			         notices: #( #( 4 7 4 'Reserved variable name' ) )).
		        (self new
			         source: '[ :super | super + 1 ]';
			         nodeAt: '0001111100044444333500';
			         isFaulty: true;
			         value: 2;
			         notices: #( #( 4 8 4 'Reserved variable name' ) )).
		        (self new
			         source: '[ :thisContext | thisContext + 1 ]';
			         nodeAt: '0001111111111100044444444444333500';
			         isFaulty: true;
			         value: 2;
			         notices: #( #( 4 14 4 'Reserved variable name' ) )).
		        (self new
			         source: '[ :Object | Object + 1 ]';
			         nodeAt: '000111111000444444333500';
			         value: 2;
			         notices: #( #( 4 9 4 'Name already defined' ) )).

		        "Backend errors"
		        "FIXME: syntax error are thrown by the compiler even in *faulty* mode.
		 FIXME: semantic analysis does not catch the error (backend issue)"
		        (self new
			         source:
				         'foo ^ [ :a1 :a2 :a3 :a4 :a5 :a6 :a7 :a8 :a9 :a10 :a11 :a12 :a13 :a14 :a15 :a16 | a1 ]';
			         nodeAt:
				         '000022333443355336633773388339933AA33BB33CC33DDD33EEE33FFF33GGG33HHH33III33JJJ333LL33';
			         isMethod: true;
			         isFaulty: true;
			         notices: #( #( 7 85 7 'Too many arguments' ) )). "Too many arguments"
		        (self new
			         source:
				         'a1: a1 a2: a2 a3: a3 a4: a4 a5: a5 a6: a6 a7: a7 a8: a8 a9: a9 a10: a10 a11: a11 a12: a12 a13: a13 a14: a14 a15: a15 a16: a16 ^ a1';
			         nodeAt:
				         '00001100000220000033000004400000550000066000007700000880000099000000AAA000000BBB000000CCC000000DDD000000EEE000000FFF000000GGG0IIJJ';
			         isMethod: true;
			         isFaulty: true;
			         notices: #( #( 1 130 1 'Too many arguments' ) )). "Too many arguments"
		        (self new
			         source: 'CodeError signal: ''false error''';
			         nodeAt: '1111111110000000002222222222222';
			         raise: CodeError;
			         skip: #testEvaluateOnErrorResume). "evaluate cannot distinguishes runtime errors and compiletime errors"
		        (self new
			         source:
				         'OCUndeclaredVariableWarning signal: ''false error''';
			         nodeAt:
				         '1111111111111111111111111110000000002222222222222';
			         raise: OCUndeclaredVariableWarning). "same, but warning are silently ignored"
		        (self new
			         source: 'RuntimeSyntaxError signal: ''false error''';
			         nodeAt: '1111111111111111110000000002222222222222';
			         raise: RuntimeSyntaxError) "runtime errors are not special" }.
	"Setup default values"
	self new
		group: #badSemantic;
		isMethod: false;
		isFaulty: false;
		isParseFaulty: false;
		applyDefaultTo: list.
	^ list
]

{ #category : #accessing }
RBCodeSnippet class >> badSimpleExpressions [
	"This list focuses simple expressions like message sends, assigments and return.
	It contains various (and possibly systematic) variations of faulty inputs (and some correct ones for good measure).
	Unless specifically testing token handling (e.g. in the scanner) try to use the formater format `formattedCode` as the source to simplify this file"

	<script: 'self styleWithError: self badSimpleExpressions'>
	| list |
	list := {
		        (self new
			         source: '1 abs';
			         nodeAt: '10000';
			         isFaulty: false;
			         value: 1).
		        (self new
			         source: '1 + 2';
			         nodeAt: '10002';
			         isFaulty: false;
			         value: 3).
		        (self new
			         source: '1 max: 2';
			         nodeAt: '10000002';
			         isFaulty: false;
			         value: 2).

		        "Missing receiver or argument in message sends.
		Note: a unary message send without a receiver will be 'correctly' mistaken as a variable, so not a parsing error"
		        "binary"
		        (self new
			         source: ' + ';
			         nodeAt: ' 00';
			         notices: #( #( 2 1 2 'Variable or expression expected' )
				            #( 4 3 4 'Variable or expression expected' ) )).
		        (self new
			         source: '1 + ';
			         nodeAt: '1000';
			         notices: #( #( 5 4 5 'Variable or expression expected' ) )).
		        (self new
			         source: ' + 2';
			         nodeAt: ' 002';
			         notices: #( #( 2 1 2 'Variable or expression expected' ) )).
		        "keywords"
		        (self new
			         source: ' hello: ';
			         nodeAt: ' 0000000';
			         notices: #( #( 2 1 2 'Variable or expression expected' )
				            #( 9 8 9 'Variable or expression expected' ) )).
		        (self new
			         source: '1 hello: ';
			         nodeAt: '100000000';
			         notices:
				         #( #( 10 9 10 'Variable or expression expected' ) )).
		        (self new
			         source: ' hello: 2';
			         nodeAt: ' 00000002';
			         notices: #( #( 2 1 2 'Variable or expression expected' ) )).
		        (self new
			         source: ' goodby: my: ';
			         nodeAt: ' 000000000000';
			         notices: #( #( 2 1 2 'Variable or expression expected' )
				            #( 10 9 10 'Variable or expression expected' )
				            #( 14 13 14 'Variable or expression expected' ) )).
		        (self new
			         source: '1 goodby: my: ';
			         nodeAt: '10000000000000';
			         notices:
				         #( #( 11 10 11 'Variable or expression expected' )
				            #( 15 14 15 'Variable or expression expected' ) )).
		        (self new
			         source: '1 goodby: 2 my: ';
			         nodeAt: '1000000000200000';
			         notices:
				         #( #( 17 16 17 'Variable or expression expected' ) )).
		        (self new
			         source: ' goodby: 2 my: ';
			         nodeAt: ' 00000000200000';
			         notices: #( #( 2 1 2 'Variable or expression expected' )
				            #( 16 15 16 'Variable or expression expected' ) )).
		        (self new
			         source: ' goodby: my: 3';
			         nodeAt: ' 0000000000003';
			         notices: #( #( 2 1 2 'Variable or expression expected' )
				            #( 10 9 10 'Variable or expression expected' ) )).
		        (self new
			         source: '1 goodby: my: 3';
			         nodeAt: '100000000000003';
			         notices:
				         #( #( 11 10 11 'Variable or expression expected' ) )).
		        (self new
			         source: ' goodby: 2 my: 3';
			         nodeAt: ' 000000002000003';
			         notices: #( #( 2 1 2 'Variable or expression expected' ) )).
		        "Combinaisons"
		        (self new
			         source: ' + foo: - ';
			         nodeAt: ' 110000044';
			         notices: #( #( 2 1 2 'Variable or expression expected' )
				            #( 4 3 4 'Variable or expression expected' )
				            #( 9 8 9 'Variable or expression expected' )
				            #( 11 10 11 'Variable or expression expected' ) )).

		        "Bad assignments"
		        (self new
			         source: 'a := ';
			         nodeAt: '20000';
			         notices: #( #( 6 5 6 'Variable or expression expected' )
				            #( 1 1 1 'Undeclared variable' ) )).
		        (self new
			         source: ':= ';
			         nodeAt: '000';
			         notices: #( #( 4 3 4 'Variable or expression expected' )
				            #( 1 3 1 'variable expected in assigment' ) )).
		        (self new
			         source: ':= 2';
			         nodeAt: '0001';
			         notices: #( #( 1 4 1 'variable expected in assigment' ) )).
		        (self new
			         source: '1:=2';
			         nodeAt: '2334';
			         formattedCode: '1. := 2';
			         notices: #( #( 1 1 2 'End of statement expected' )
				            #( 2 4 2 'variable expected in assigment' ) )).

		        "Bad cascades"
		        (self new
			         source: ';';
			         nodeAt: '0';
			         formattedCode: ' ; ';
			         notices: #( #( 1 0 1 'Variable or expression expected' )
				            #( 1 0 1 'Message expected' )
				            #( 2 1 2 'Cascade message expected' ) )).
		        (self new
			         source: '1;foo';
			         nodeAt: '43333';
			         formattedCode: '1 ; foo';
			         notices: #( #( 1 1 2 'Message expected' ) )).
		        (self new
			         source: '1;';
			         nodeAt: '20';
			         formattedCode: '1 ; ';
			         notices:
				         #( #( 1 1 2 'Message expected' )
				            #( 3 2 3 'Cascade message expected' ) )).
		        (self new
			         source: '1 sign;';
			         nodeAt: '2111110';
			         formattedCode: '1 sign; ';
			         notices: #( #( 8 7 8 'Cascade message expected' ) )).
		        (self new
			         source: '1 foo:;bar';
			         nodeAt: '5111114444';
			         formattedCode: '1 foo: ; bar';
			         notices: #( #( 7 6 7 'Variable or expression expected' ) )). "The cascade is correct here. It's a simple error of a missing argument"
		        (self new
			         source: '1 foo;2';
			         nodeAt: '4333326';
			         formattedCode: '1 foo; . 2';
			         notices: #( #( 7 6 7 'Cascade message expected' )
				            #( 1 6 7 'End of statement expected' ) )).
		        (self new
			         source: '(1 sign: 2);bar';
			         nodeAt: '676666666865555';
			         formattedCode: '(1 sign: 2) ; bar';
			         notices: #( #( 1 11 12 'Message expected' ) )).
		        (self new
			         source: '(1 sign);bar';
			         nodeAt: '565555554444';
			         formattedCode: '1 sign ; bar';
			         notices: #( #( 1 8 9 'Message expected' ) )). "FIXME the parentheses are lost, and this changes the meaning"
		        "Longer cascade"
		        (self new
			         source: ';;';
			         nodeAt: '00';
			         formattedCode: ' ; ; ';
			         notices: #( #( 1 0 1 'Variable or expression expected' )
				            #( 1 0 1 'Message expected' )
				            #( 2 1 2 'Cascade message expected' )
				            #( 3 2 3 'Cascade message expected' ) )).
		        (self new
			         source: '1 sign;;bar';
			         nodeAt: '51111144444';
			         formattedCode: '1 sign; ; bar';
			         notices: #( #( 8 7 8 'Cascade message expected' ) )).

		        "Bad returns"
		        (self new
			         source: '^ ';
			         nodeAt: '00';
			         notices: #( #( 3 2 3 'Variable or expression expected' ) )).
		        (self new
			         source: '1+^2';
			         nodeAt: '3256';
			         formattedCode: '1 + . ^ 2';
			         notices: #( #( 3 2 3 'Variable or expression expected' )
				            #( 1 2 3 'End of statement expected' ) )).
		        (self new
			         source: '1 foo: ^2';
			         nodeAt: '322222256';
			         formattedCode: '1 foo: . ^ 2';
			         notices: #( #( 8 7 8 'Variable or expression expected' )
				            #( 1 7 8 'End of statement expected' ) )).
		        (self new
			         source: '(^1)';
			         nodeAt: '1453';
			         formattedCode: '( . ^ 1 )';
			         notices: #( #( 2 1 2 'Variable or expression expected' )
				            #( 1 1 2 ''')'' expected' )
				            #( 2 4 4 'Missing opener for closer: )' ) )). "^ can only appear a the begin of a statement, so a random ^ cause an unfinished statement error"
		        (self new
			         source: '^^1';
			         nodeAt: '245';
			         formattedCode: '^ . ^ 1';
			         notices: #( #( 2 1 2 'Variable or expression expected' )
				            #( 1 1 2 'End of statement expected' ) )). "Same spirit"
		        (self new
			         source: '[ ^ 1 ]';
			         nodeAt: '0022300';
			         isFaulty: false;
			         raise: BlockCannotReturn). "when the block is evaluated, the method is already gone."
		        (self new
			         source: '{ ^ 1 }';
			         nodeAt: '0011200';
			         isFaulty: false;
			         value: 1). "I did not expect this one to be legal"
		        (self new
			         source: '#(^1)';
			         nodeAt: '00120';
			         formattedCode: '#( #''^'' 1 )';
			         isFaulty: false;
			         value: #( #'^' 1 )). "Obviously..."
		        (self new
			         source: '#[ ^ 1 ]';
			         nodeAt: '00010200';
			         notices: #( #( 4 4 4 '8-bit integer expected' ) )).

		        "Unreachable code (warnings)"
		        "Unreachable analysis is very simple and targets statement that directly follows a return statement."
		        "Note that faulty code can still be executed without a RuntimeSyntaxError"
		        (self new
			         source: '^ 1. 2. ^ 3';
			         nodeAt: '11200300445';
			         isFaulty: false;
			         value: 1;
			         notices: #( #( 6 6 6 'Unreachable statement' ) )).
		        (self new
			         source: '[ ^ 1. 2. ^ 3 ]';
			         nodeAt: '002231141155600';
			         isFaulty: false;
			         raise: BlockCannotReturn;
			         notices: #( #( 8 8 8 'Unreachable statement' ) )). "like [^1]"
		        (self new
			         source: '{ ^ 1. 2. ^ 3 }';
			         nodeAt: '001120030044500';
			         isFaulty: false;
			         value: 1;
			         notices: #( #( 8 8 8 'Unreachable statement' ) )).
		        (self new
			         source: '[ ^ 1 ]. 2. ^ 3';
			         nodeAt: '113341100500667';
			         isFaulty: false;
			         value: 3).
		        (self new
			         source: '{ ^ 1 }. 2. ^ 3';
			         nodeAt: '112231100400556';
			         isFaulty: false;
			         value: 1). "This one could have been..."
		        (self new
			         source: 'true ifTrue: [ ^ 1 ] ifFalse: [ ^ 2 ]. ^ 3';
			         nodeAt: '2222111111111335563311111111117799A7700BBC';
			         isFaulty: false;
			         value: 1) "Not *syntactic* enough" }.

	"Setup default values"
	self new
		group: #badSimpleExpressions;
		isMethod: false;
		isFaulty: true;
		applyDefaultTo: list.
	^ list
]

{ #category : #accessing }
RBCodeSnippet class >> badTokens [
	"This list focuses on bad literal, malformed tokens, unexpected character and other scanner related issue.
	It contains various (and possibly systematic) variations of faulty inputs (and some correct ones for good measure).
	Unless specifically testing token handling (e.g. in the scanner) try to use the formater format `formattedCode` as the source to simplify this file"

	<script: 'self styleWithError: self badTokens'>
	| list |
	list := {
		        (self new
			         source: '#';
			         nodeAt: '0';
			         notices: #( #( 1 1 2 'Literal expected' ) )).
		        (self new
			         source: '$';
			         nodeAt: '0';
			         notices: #( #( 1 1 2 'Character expected' ) )).
		        (self new
			         source: ':';
			         nodeAt: '0';
			         notices: #( #( 1 1 1 'Unexpected token' ) )).
		        (self new
			         source: '';
			         nodeAt: '';
			         isFaulty: false). "emptyness is ok"

		        "Comments"
		        "EFFormater is not the best here :("
		        (self new
			         source: '"" ';
			         nodeAt: '';
			         isFaulty: false).
		        (self new
			         source: '"nothing" ';
			         nodeAt: '';
			         isFaulty: false).
		        (self new
			         source: '"com"1"ment"';
			         nodeAt: '     0';
			         formattedCode: '1';
			         isFaulty: false;
			         value: 1). "The comments are in the AST, the formatter just do not know to show them because we format only the node and not the whole method body"
		        (self new
			         source: '"a" 1 "b". "c" 2 "d"';
			         nodeAt: '    100000000005';
			         formattedCode: '1. "a" "b" "c" 2 "d"';
			         isFaulty: false;
			         value: 2). "a and b moved around. Formatter issue. FIXME?"
		        (self new
			         source: '"unfinished';
			         nodeAt: '00000000000';
			         notices: #( #( 1 11 12 'Unmatched " in comment.' ) )).
		        (self new
			         source: '"also unfinished""';
			         nodeAt: '000000000000000000';
			         notices: #( #( 1 18 19 'Unmatched " in comment.' ) )).
		        (self new
			         source: '"';
			         nodeAt: '0';
			         notices: #( #( 1 1 2 'Unmatched " in comment.' ) )).
		        (self new
			         source: '"""';
			         nodeAt: '000';
			         notices: #( #( 1 3 4 'Unmatched " in comment.' ) )).


		        "Bad string literal"
		        "Note: the only cases are the missing closing quotes since everything inside is captured as is and there is no escape sequences or interpolation (yet?)"
		        (self new
			         source: '''hello';
			         nodeAt: '000000';
			         notices: #( #( 1 6 7 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '''hello''''world';
			         nodeAt: '0000000000000';
			         notices:
				         #( #( 1 13 14 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '''';
			         nodeAt: '0';
			         notices: #( #( 1 1 2 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '''hello''''';
			         nodeAt: '00000000';
			         notices: #( #( 1 8 9 'Unmatched '' in string literal.' ) )). "unclosed string that ends with an escaped quote"

		        "Bad symbol literal"
		        (self new
			         source: '#1';
			         nodeAt: '12';
			         formattedCode: '#. 1';
			         notices: #( #( 1 1 2 'Literal expected' ) )). "Become a bad sequence"
		        (self new
			         source: '#1r0';
			         nodeAt: '1322';
			         formattedCode: '#. 1 r0';
			         notices:
				         #( #( 1 1 2 'Literal expected' )
				            #( 2 2 3 'an integer greater than 1 as valid radix expected' ) )). "Two bad sequences"
		        (self new
			         source: '##';
			         nodeAt: '00';
			         notices: #( #( 1 2 3 'Literal expected' ) )).
		        "Note: if quotes, same thing than strings"
		        (self new
			         source: '#''hello';
			         nodeAt: '0000000';
			         notices: #( #( 1 7 8 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '#''hello''''world';
			         nodeAt: '00000000000000';
			         notices:
				         #( #( 1 14 15 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '#''';
			         nodeAt: '00';
			         notices: #( #( 1 2 3 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '#''hello''''';
			         nodeAt: '000000000';
			         notices:
				         #( #( 1 9 10 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '###''hello';
			         nodeAt: '000000000';
			         notices:
				         #( #( 1 9 10 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '###''hello''''world';
			         nodeAt: '0000000000000000';
			         notices:
				         #( #( 1 16 17 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '###''';
			         nodeAt: '0000';
			         notices: #( #( 1 4 5 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '###''hello''''';
			         nodeAt: '00000000000';
			         notices:
				         #( #( 1 11 12 'Unmatched '' in string literal.' ) )).

		        "Bad numeric literal"
		        "Note: currently there is only 2 cases or bad numeric literal, both related to bad radix"
		        (self new
			         source: '2r';
			         nodeAt: '00';
			         notices:
				         #( #( 1 2 3 'a digit between 0 and 1 expected' ) )).
		        (self new
			         source: '2rx';
			         nodeAt: '110';
			         formattedCode: '2r x';
			         notices:
				         #( #( 1 2 3 'a digit between 0 and 1 expected' ) )). "a bad number followed by a unary message send"
		        (self new
			         source: '2r3';
			         nodeAt: '112';
			         formattedCode: '2r. 3';
			         notices:
				         #( #( 1 2 3 'a digit between 0 and 1 expected' ) )). "a bad number followed by a number, causing a case of unfinished sequence"
		        (self new
			         source: '0r';
			         nodeAt: '10';
			         formattedCode: '0 r';
			         notices:
				         #( #( 1 1 2 'an integer greater than 1 as valid radix expected' ) )).
		        (self new
			         source: '000rx';
			         nodeAt: '11100';
			         formattedCode: '000 rx';
			         notices:
				         #( #( 1 3 4 'an integer greater than 1 as valid radix expected' ) )).
		        (self new
			         source: '000r1';
			         nodeAt: '11100';
			         formattedCode: '000 r1';
			         notices:
				         #( #( 1 3 4 'an integer greater than 1 as valid radix expected' ) )).
		        (self new
			         source: '3r12345';
			         nodeAt: '2222333';
			         formattedCode: '3r12. 345';
			         notices: #( #( 1 4 5 'End of statement expected' ) )).

		        "These ones are correct, the number parser is very prermisive (except for radix, see above)"
		        (self new
			         source: '1.';
			         nodeAt: '0';
			         formattedCode: '1';
			         isFaulty: false;
			         value: 1).
		        (self new
			         source: '1.1.1';
			         nodeAt: '11102';
			         formattedCode: '1.1. 1';
			         isFaulty: false;
			         value: 1).
		        (self new
			         source: '1a';
			         nodeAt: '10';
			         formattedCode: '1 a';
			         isFaulty: false;
			         messageNotUnderstood: #a).
		        (self new
			         source: '1a1a1';
			         nodeAt: '10000';
			         formattedCode: '1 a1a1';
			         isFaulty: false;
			         messageNotUnderstood: #a1a1).
		        (self new
			         source: '1e';
			         nodeAt: '10';
			         formattedCode: '1 e';
			         isFaulty: false;
			         messageNotUnderstood: #e).
		        (self new
			         source: '1e1e1';
			         nodeAt: '11100';
			         formattedCode: '1e1 e1';
			         isFaulty: false;
			         messageNotUnderstood: #e1).
		        (self new
			         source: '1s';
			         nodeAt: '00';
			         isFaulty: false;
			         value: 1s0). "ScaledDecimal is a thing (!) that have literals (!!) inconsistent with '1e' (!!!)"
		        (self new
			         source: '1s1s1';
			         nodeAt: '11100';
			         formattedCode: '1s1 s1';
			         isFaulty: false;
			         messageNotUnderstood: #s1).
		        (self new
			         source: '10r89abcd';
			         nodeAt: '111110000';
			         formattedCode: '10r89 abcd';
			         isFaulty: false;
			         messageNotUnderstood: #abcd).
		        (self new
			         source: '12r89abcd';
			         nodeAt: '111111100';
			         formattedCode: '12r89ab cd';
			         isFaulty: false;
			         messageNotUnderstood: #cd).
		        (self new
			         source: '36r1halt';
			         nodeAt: '00000000';
			         isFaulty: false;
			         value: 2486513). "ahah"

		        "Bad characters"
		        "Pharo is Unicode aware."
		        "$Δ isLetter >>> true" "$Δ asInteger >>> 16r0394" "Greek Capital Letter Delta"
		        "$ə isLetter >>> true" "$ə asInteger >>> 16r0259" "Latin Small Letter Schwa"
		        (self new
			         source: 'Δə';
			         nodeAt: '00';
			         isParseFaulty: false;
			         isFaultyMinusUndeclared: false;
			         raise: UndeclaredVariableRead;
			         notices: #( #( 1 2 1 'Undeclared variable' ) )). "valid identifier"
		        (self new
			         source: '| Δə | Δə := 1. Δə + 1';
			         nodeAt: '0011000442222300665557';
			         isFaulty: false;
			         value: 2).

		        "$± isSpecial >>> true" "$± asInteger >>> 16r00B1" "Plus Minus Sign" "Only a few unicode characters are isSpecial in Pharo"
		        (self new
			         source: '1 ± 1';
			         nodeAt: '10002';
			         isFaulty: false;
			         messageNotUnderstood: #±). "Valid binary operator, but not implemented"
		        "$→ isSpecial >>> false" "$→ asInteger hex >>> 16r2192" "Rightwards Arrow"
		        (self new
			         source: '→';
			         nodeAt: '0';
			         notices: #( #( 1 1 1 'Unknown character' ) )). "Unknown character. Not isSpecial"
		        "$٠ isDigit >>> true" "$٠ asInteger >>> 16r0660" "Arabic-indic Digit Zero"
		        (self new
			         source: '٠';
			         nodeAt: '0';
			         notices: #( #( 1 1 1 'Unknown character' ) )). "Unknown character. Not a valid number (basic ASCII only for numbers!)"
		        "Currently in Pharo, there is no 'isSeparator' character outside the ASCII range"
		        "Character nbsp isSeparator >>> false" "Even the standard nbsp"
		        (self new
			         source: Character nbsp asString;
			         notices: #( #( 1 1 1 'Unknown character' ) )). "Unknown character. Not isSeparator"
		        (self new
			         source: '$→';
			         nodeAt: '00';
			         isFaulty: false;
			         value: $→).
		        (self new
			         source: '''Δ→ə''';
			         nodeAt: '00000';
			         isFaulty: false;
			         value: 'Δ→ə').
		        (self new
			         source: '"Δ→ə" ';
			         nodeAt: '';
			         isFaulty: false).
		        (self new
			         source: '#''Δ→ə''';
			         nodeAt: '000000';
			         isFaulty: false;
			         value: #'Δ→ə').
		        (self new
			         source: '#Δə';
			         nodeAt: '000';
			         formattedCode: '#''Δə''';
			         isFaulty: false;
			         value: #'Δə').
		        (self new
			         source: '#(Δ→ə)';
			         nodeAt: '001230';
			         formattedCode: '#( Δ → ə )';
			         notices: #( #( 4 4 4 'Unknown character' ) )). "This one is faulty because → is a parse error"
		        (self new
			         source: '#→';
			         nodeAt: '12';
			         formattedCode: '#. →';
			         notices:
				         #( #( 1 1 2 'Literal expected' )
				            #( 2 2 2 'Unknown character' ) )) "Two independent errors" }.
	"Setup default values"
	self new
		group: #badTokens;
		isMethod: false;
		isFaulty: true;
		applyDefaultTo: list.
	^ list
]

{ #category : #accessing }
RBCodeSnippet class >> badVariableAndScopes [
	"Test various combinaisons of error and warnings on variables and scope nesting"

	<script: 'self styleWithError: self badVariableAndScopes'>
	| list |
	list := {
		        (self new
			         source: 'a := a. { [ :a }. a := a';
			         nodeAt: '311112004455555766A88889';
			         formattedCode: 'a := a. { [ :a | }. a := a';
			         raise: UndeclaredVariableRead;
			         notices:
				         #( #( 6 6 6 'Undeclared variable' )
				            #( 1 1 1 'Undeclared variable' )
				            #( 16 16 16 'Missing opener for closer: }' )
				            #( 11 24 25 ''']'' expected' )
				            #( 9 24 25 '''}'' expected' ) )). "OK, but maybe the `}` could close the `{` and consider the `[` unfinished?"
		        (self new
			         source: 'a := a. [ :a [ :a. a := a';
			         nodeAt: '3111120044444666666A88889';
			         formattedCode: 'a := a. [ :a | [ :a | a := a';
			         raise: UndeclaredVariableRead;
			         notices:
				         #( #( 6 6 6 'Undeclared variable' )
				            #( 1 1 1 'Undeclared variable' )
				            #( 14 25 26 ''']'' expected' )
				            #( 9 25 26 ''']'' expected' ) )).
		        (self new
			         source: 'a := a. [ :a [ :a ]. a := a';
			         nodeAt: '311112004444466676655B9999A';
			         formattedCode: 'a := a. [ :a | [ :a | ]. a := a';
			         raise: UndeclaredVariableRead;
			         notices:
				         #( #( 6 6 6 'Undeclared variable' )
				            #( 1 1 1 'Undeclared variable' )
				            #( 9 27 28 ''']'' expected' ) )).
		        (self new
			         source: 'a := a. { [ :a | }. a := a';
			         nodeAt: '31111200445555555766A88889';
			         raise: UndeclaredVariableRead;
			         notices:
				         #( #( 6 6 6 'Undeclared variable' )
				            #( 1 1 1 'Undeclared variable' )
				            #( 18 18 18 'Missing opener for closer: }' )
				            #( 11 26 27 ''']'' expected' )
				            #( 9 26 27 '''}'' expected' ) )).
		        (self new
			         source: 'a := a. { [ :a | a := a }. a := a';
			         nodeAt: '31111200445555555A888897766DBBBBC';
			         raise: UndeclaredVariableRead;
			         notices:
				         #( #( 6 6 6 'Undeclared variable' )
				            #( 1 1 1 'Undeclared variable' )
				            #( 18 25 25 'Missing opener for closer: }' )
				            #( 11 33 34 ''']'' expected' )
				            #( 9 33 34 '''}'' expected' ) )).
		        (self new
			         source: 'a := a. [ | a a := a ]. a := a';
			         nodeAt: '3111120044667685999A4400DBBBBC';
			         formattedCode: 'a := a. [ | a a | . := a ]. a := a';
			         raise: UndeclaredVariableRead;
			         notices:
				         #( #( 6 6 6 'Undeclared variable' )
				            #( 1 1 1 'Undeclared variable' )
				            #( 11 15 17 '''|'' or variable expected' )
				            #( 17 20 17 'variable expected in assigment' ) )).
		        (self new
			         source: 'a := a. [ :a a ]. a := a';
			         nodeAt: '311112004445484400B9999A';
			         formattedCode: 'a := a. [ :a | a ]. a := a';
			         raise: UndeclaredVariableRead;
			         notices:
				         #( #( 6 6 6 'Undeclared variable' )
				            #( 1 1 1 'Undeclared variable' )
				            #( 14 13 14 '''|'' or parameter expected' )
				            #( 24 24 24 'Undeclared variable' )
				            #( 19 19 19 'Undeclared variable' ) )).
		        (self new
			         source: 'a := a. [ :a | | a a := a ]. a := a';
			         nodeAt: '311112004445444778796AAAB4400ECCCCD';
			         formattedCode: 'a := a. [ :a | | a a | . := a ]. a := a';
			         raise: UndeclaredVariableRead;
			         notices:
				         #( #( 6 6 6 'Undeclared variable' )
				            #( 1 1 1 'Undeclared variable' )
				            #( 16 20 22 '''|'' or variable expected' )
				            #( 22 25 22 'variable expected in assigment' ) )).

		        "Chains of shadowing"
		        (self new
			         source: '[ :a :a :b | | a a b | a + a + b ]';
			         nodeAt: '00010020030004454647444A999B888C00';
			         isFaulty: false;
			         value: 4;
			         notices:
				         #( #( 7 7 7 'Name already defined' )
				            #( 16 16 16 'Name already defined' )
				            #( 16 16 16 'Unused variable' )
				            #( 18 18 18 'Name already defined' )
				            #( 20 20 20 'Name already defined' )
				            #( 24 24 24 'Unitialized variable' )
				            #( 28 28 28 'Unitialized variable' )
				            #( 32 32 32 'Unitialized variable' ) )).
		        (self new
			         source:
				         'foo: a x: a y: b [ :a :a :b | | a a b | a + a + b ]';
			         nodeAt:
				         '00000100002000030555655755855599A9B9C999FEEEGDDDH55';
			         isMethod: true;
			         isFaulty: false;
			         notices: #( #( 11 11 11 'Name already defined' )
				            #( 21 21 21 'Name already defined' )
				            #( 24 24 24 'Name already defined' )
				            #( 27 27 27 'Name already defined' )
				            #( 33 33 33 'Name already defined' )
				            #( 33 33 33 'Unused variable' )
				            #( 35 35 35 'Name already defined' )
				            #( 37 37 37 'Name already defined' )
				            #( 41 41 41 'Unitialized variable' )
				            #( 45 45 45 'Unitialized variable' )
				            #( 49 49 49 'Unitialized variable' ) )).
		        (self new
			         source: '[ :a :a :b | | a a b | a + a + b';
			         nodeAt: '00000000000001121314111766685559';
			         notices:
				         #( #( 16 16 16 'Unused variable' )
				            #( 18 18 18 'Name already defined' )
				            #( 24 24 24 'Unitialized variable' )
				            #( 28 28 28 'Unitialized variable' )
				            #( 32 32 32 'Unitialized variable' )
				            #( 1 32 33 ''']'' expected' ) )).
		        (self new
			         source:
				         'foo: a x: a y: b [ :a :a :b | | a a b | a + a + b';
			         nodeAt:
				         '0000010000200003055555555555556676869666CBBBDAAAE';
			         isMethod: true;
			         notices: #( #( 11 11 11 'Name already defined' )
				            #( 33 33 33 'Name already defined' )
				            #( 33 33 33 'Unused variable' )
				            #( 35 35 35 'Name already defined' )
				            #( 37 37 37 'Name already defined' )
				            #( 41 41 41 'Unitialized variable' )
				            #( 45 45 45 'Unitialized variable' )
				            #( 49 49 49 'Unitialized variable' )
				            #( 18 49 50 ''']'' expected' ) )) }.
	"Setup default values"
	self new
		group: #badVariableAndScopes;
		isMethod: false;
		isFaulty: true;
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
RBCodeSnippet class >> styleWithError: snippets [
	"Display all snippets in a big styled text with compilation error messages and error nodes.
	Each compilation error message and each error node is displayed indenpendently."

	<script: 'self styleWithError: self allSnippets'>

	| bigtext |
	bigtext := Text new.
	snippets do: [ :each |
		| text ast |
		bigtext ifNotEmpty: [ bigtext append: String cr ].

		"Get the original styled text"
		text := each styledText.
		bigtext append: text.

		"Try to compile it and collect error messages"
		each compileOnError: [ :exception |
				"In case of error, insert error message (like an old-school smalltalk compiler)"
				text
					replaceFrom: exception position
					to: exception position - 1
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

{ #category : #updating }
RBCodeSnippet class >> updateSnippets: snippets [
	"This script will update all definitions of all snippets.
	Beware of code loss and double check the generated definitions."

	"I did not manage to update all in a single transformation (with a preview!)
	So do them one by one with a progress bar :("

	<script: 'self updateSnippets: self allSnippets'>
	snippets
		do: [ :snippet |
			snippet definitionRefactoring ifNotNil: [ :refactoring |
				refactoring
					transform;
					execute ] ]
		displayingProgress: 'Updating snippets...'
]

{ #category : #accessing }
RBCodeSnippet >> applyDefaultTo: aCollection [

	aCollection do: [ :each |
		each default: self.
		each isMethod ifNil: [ each isMethod: self isMethod ].
		each isFaulty ifNil: [ each isFaulty: self isFaulty ].
		each isParseFaulty ifNil: [ each isParseFaulty: self isParseFaulty ].
		each isParseFaulty ifNil: [ each isParseFaulty: each isFaulty ].
		each isFaultyMinusUndeclared ifNil: [ each isFaultyMinusUndeclared: each isFaulty ].
		each formattedCode ifNil: [ each formattedCode: each source ].
	]
]

{ #category : #accessing }
RBCodeSnippet >> default: aRBCodeSnippet [

	default := aRBCodeSnippet
]

{ #category : #updating }
RBCodeSnippet >> definitionRefactoring [
	"A transformation to update the definition of self in the correct class side method"

	| node old new newast |
	node := self searchDefinition.
	node ifNil: [ ^ nil ].
	self updateExpectations.

	"Check if the new code is different from the old one"
	old := node sourceCode.
	new := self dumpDefinition.
	newast := RBParser parseFaultyExpression: new.
	newast parent: node parent. "Force same context, so formattedCode is reliable"
	node formattedCode = newast formattedCode ifTrue: [ ^ nil ].

	^ RBReplaceSubtreeTransformation
		  replace: old
		  to: new
		  inMethod: node methodNode selector
		  inClass: node methodNode methodClass
]

{ #category : #updating }
RBCodeSnippet >> dumpDefinition [
	"The definition of the snippet (according to the current values)
	as it should appears in the class side methods."

	^ String streamContents: [ :aStream |
		  aStream
			  nextPutAll: '(self new';
			  nextPutAll: ' source: ';
			  print: self source.
		  aStream
			  nextPutAll: '; nodeAt: ';
			  print: self nodePositions.
		  self formattedCode = self source ifFalse: [
			  aStream
				  nextPutAll: '; formattedCode: ';
				  print: self formattedCode ].
		  self isMethod = default isMethod ifFalse: [
			  aStream
				  nextPutAll: '; isMethod: ';
				  print: self isMethod ].
		  self isFaulty = default isFaulty ifFalse: [
			  aStream
				  nextPutAll: '; isFaulty: ';
				  print: self isFaulty ].
		  (default isParseFaulty
			   ifNotNil: [ self isParseFaulty ~= default isParseFaulty ]
			   ifNil: [ self isParseFaulty ~= self isFaulty ]) ifTrue: [
			  aStream
				  nextPutAll: '; isParseFaulty: ';
				  print: self isParseFaulty ].
		  self isFaultyMinusUndeclared ~= self isFaulty ifTrue: [
			  aStream
				  nextPutAll: '; isFaultyMinusUndeclared: ';
				  print: self isFaultyMinusUndeclared ].
		  self dumpDefinitionValue: aStream.
		  self numberOfCritiques ifNotNil: [
			  aStream
				  nextPutAll: '; numberOfCritiques: ';
				  print: self numberOfCritiques ].
		  self notices isEmptyOrNil ifFalse: [
			  aStream
				  nextPutAll: '; notices: ';
				  print: self notices ].
		  self skippedTests ifNotNil: [
			  self skippedTests do: [ :each |
				  aStream
					  nextPutAll: '; skip: ';
					  print: each ] ].
		  aStream nextPut: $) ]
]

{ #category : #updating }
RBCodeSnippet >> dumpDefinitionValue: aStream [

	self messageNotUnderstood ifNotNil: [ :val |
		aStream
			nextPutAll: '; messageNotUnderstood: ';
			print: val.
		^ self ].

	self raise ifNotNil: [ :val |
		aStream
			nextPutAll: '; raise: ';
			print: val.
		^ self ].

	(self value isNotNil or: [ self isFaulty and: [ self hasValue ] ])
		ifTrue: [
			aStream
				nextPutAll: '; value: ';
				print: self value ]
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
RBCodeSnippet >> hasAllNotices: someNotices [

	(self notices ifNil: [0] ifNotNil: [self notices size]) = someNotices size ifFalse: [ ^ false ].
	^ someNotices allSatisfy: [ :each | self hasNotice: each ]
]

{ #category : #testing }
RBCodeSnippet >> hasNotice: aNotice [

	self notices ifNil: [ ^ false ].
	^ self notices anySatisfy: [ :each |
		aNotice node start = each first
		& (aNotice node stop = each second)
		& (aNotice position = each third)
		& (aNotice messageText = each fourth) ]
]

{ #category : #testing }
RBCodeSnippet >> hasNotice: aString at: anInteger [

	self notices ifNil: [ ^ false ].

	^ self notices anySatisfy: [ :each |
		  each third = anInteger and: [ each fourth = aString ] ]
]

{ #category : #testing }
RBCodeSnippet >> hasUndeclared [

	self notices ifNil: [ ^ false ].
	^ self notices anySatisfy: [ :n | n fourth = 'Undeclared variable' ]
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

{ #category : #inspecting }
RBCodeSnippet >> inspectionSource [
	<inspectorPresentationOrder: 35 title: 'Source'>

	^ self searchDefinition inspectionSource
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
RBCodeSnippet >> isFaultyMinusUndeclared [

	^ isFaultyMinusUndeclared
]

{ #category : #accessing }
RBCodeSnippet >> isFaultyMinusUndeclared: anObject [

	isFaultyMinusUndeclared := anObject
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
RBCodeSnippet >> nodeAt: anObject [
	"Synonym for `bestNodes:` used to be aligned with `source:`"

	nodePositions := anObject
]

{ #category : #accessing }
RBCodeSnippet >> nodePositions [

	^ nodePositions
]

{ #category : #accessing }
RBCodeSnippet >> nodePositions: anObject [

	nodePositions := anObject
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

{ #category : #updating }
RBCodeSnippet >> searchDefinition [
	"Get the AST node defining self in the class side method.
	The source code is used as a key to distinguishes other snippets.
	Assume the AST of the class side method respect the format."

	| ast candidate |
	candidate := OrderedCollection new.
	ast := (self class class >> default group) ast.
	ast nodesDo: [ :node |
		| parent |
		(node isLiteralNode and: [
			 parent := node parent.
			 node value = self source and: [
				 parent isMessage and: [ parent selector = #source: ] ] ])
			ifTrue: [
				parent isCascaded ifTrue: [ parent := parent parent ].
				candidate add: parent ] ].
	^ candidate size = 1
		  ifTrue: [ candidate first ]
		  ifFalse: [ nil ]
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

{ #category : #updating }
RBCodeSnippet >> updateDefinition [
	"Do the update of the definition of self in the method (class-side) that defines it"

	self definitionRefactoring ifNotNil: [ :transformation |
		transformation
			transform;
			execute ]
]

{ #category : #updating }
RBCodeSnippet >> updateExpectations [
	"Update the value of expectations according what tools produce.
	Do not blindly trust it, as a tool cannot judge itself."

	| ast |
	ast := self parse.
	self formattedCode: ast formattedCode withSeparatorsCompacted.
	self isParseFaulty: ast isFaulty.
	self nodePositions:
		(' ' repeat: ast start - 1) , ast asPositionDebugString.
	ast := self doSemanticAnalysis.
	self isFaulty: ast isFaulty.
	self isFaultyMinusUndeclared: self isFaulty
		& (ast allErrorNotices allSatisfy: #isUndeclaredNotice) not.
	self notices: (ast allNotices
			 collect: [ :each |
				 {
					 each node start.
					 each node stop.
					 each position.
					 each messageText } ]
			 as: Array)
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
