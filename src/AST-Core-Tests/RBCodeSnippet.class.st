"
Small pieces of source code used used to test various AST based tools.

See `RBCodeSnippet allSnippets` for a various collection of instances.

* source <String> the source code of the snippet
* isScripting <Boolean> the source is for a script (without pattern or pragmas)
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
	#name : 'RBCodeSnippet',
	#superclass : 'Object',
	#instVars : [
		'source',
		'isScripting',
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
		'nodePositions',
		'styledPattern'
	],
	#category : 'AST-Core-Tests-Snippets',
	#package : 'AST-Core-Tests',
	#tag : 'Snippets'
}

{ #category : 'accessing' }
RBCodeSnippet class >> allSnippets [

	^ {
		  self badAnnotations.
		  self badComments.
		  self badSimpleExpressions.
		  self badExpressions.
		  self badTokens.
		  self badMethods.
		  self badScanner.
		  self badSemantic.
		  self badPositions.
		  self badVariableAndScopes } flattened
]

{ #category : 'accessing' }
RBCodeSnippet class >> badAnnotations [

	<script: 'self styleWithError: self badAnnotations'>
	| list |
	list := {
		        (self new
			         source: '@';
			         nodeAt: '0';
			         styled: '<';
			         isParseFaulty: false;
			         notices: #( #( 1 1 1 'Unexpected token' ) )).
		        (self new
			         source: '@5';
			         nodeAt: '23';
			         styled: '<X';
			         formattedCode: '@. 5';
			         notices:
				         #( #( 1 1 1 'Unexpected token' )
				            #( 1 1 2 'End of statement expected' ) )).
		        (self new
			         source: 'a := @';
			         nodeAt: '200001';
			         styled: 'u    <';
			         isParseFaulty: false;
			         notices:
				         #( #( 6 6 6 'Unexpected token' )
				            #( 1 1 1 'Undeclared variable' ) )).
		        (self new
			         source: '^ @';
			         nodeAt: '001';
			         styled: '^ <';
			         isParseFaulty: false;
			         notices: #( #( 3 3 3 'Unexpected token' ) )).
		        (self new
			         source: '@foo';
			         nodeAt: '1000';
			         styled: '<<<<';
			         formattedCode: '@ foo';
			         isParseFaulty: false;
			         notices: #( #( 1 4 1 'Unknown annotation' ) )).
		        (self new
			         source: '@foo:';
			         nodeAt: '10000';
			         styled: '<<<<<';
			         formattedCode: '@ foo: ';
			         notices: #( #( 6 5 6 'Variable or expression expected' )
				            #( 1 5 1 'Unknown annotation' ) )).
		        (self new
			         source: '@foo:5';
			         nodeAt: '100002';
			         styled: '<<<<<n';
			         formattedCode: '@ foo: 5';
			         isParseFaulty: false;
			         notices: #( #( 1 6 1 'Unknown annotation' ) )) }.

	"Setup default values"
	self new
		group: #badAnnotations;
		isScripting: true;
		isFaulty: true;
		applyDefaultTo: list.
	^ list
]

{ #category : 'accessing' }
RBCodeSnippet class >> badComments [
	"EFFormater is not the best here :("

	<script: 'self styleWithError: self badComments'>
	| list |
	list := {
		        (self new
			         source: '"" ';
			         nodeAt: '22 ';
			         styled: '"" ').
		        (self new
			         source: '"nothing" ';
			         nodeAt: '222222222 ';
			         styled: '""""""""" ').
		        (self new
			         source: '"com"1"ment"';
			         nodeAt: '333330444444';
			         styled: '"""""n""""""';
			         formattedCode: '1';
			         value: 1). "The comments are in the AST, the formatter just do not know to show them because we format only the node and not the whole method body"

		        "Comments betwen all tokens"
		        "Unfortunately, EFFormater loose a lot of comments :("
		        (self new
			         source: '"a" 1 "b". "c" 2 "d"';
			         nodeAt: '555 106660077708 AAA';
			         styled: '""" n """. """ n """';
			         formattedCode: '1. "a" "b" "c" 2 "d"';
			         value: 2). "a and b moved around. Formatter issue. FIXME?"
		        (self new
			         source: ' "z" foo "a" 1 "b". "c" ^ 2 "d" ';
			         nodeAt: '0DDD00000EEE04377733888399A0CCC0';
			         styled: ' """ ppp """ n """. """ ^ n """ ';
			         formattedCode: 'foo "z" "a" 1. "b" "c" ^ 2 "d"';
			         isScripting: false;
			         value: 2). "z and b moved around. Formatter issue. FIXME?"
		        (self new
			         source: '"a" 1 "b" max: "c" 2 "d" + "e" 3 "f" abs "g"';
			         nodeAt: '444 10555000000AAA076BBB666HHH6ECIIICCCC JJJ';
			         styled: '""" n """ ssss """ n """ s """ n """ sss """';
			         formattedCode: '1 max: 2 + 3 abs';
			         value: 5).
		        (self new
			         source: '"a" ( "b" ( "c" 1 "d" ) "e" ) "f"';
			         nodeAt: '777 00888000999000AAA000BBB00 CCC';
			         styled: '""" 0 """ 1 """ n """ 1 """ 0 """';
			         formattedCode: '1';
			         value: 1).
		        (self new
			         source: ' "a" 1 "b" max: "c" 2 "d" ; "e" min: "f" 3 "g" ';
			         nodeAt: ' HHH E1III111111AAA17CBBBCCCOOOCCCCCCMMMCJ NNN ';
			         styled: ' """ n """ ssss """ n """ ; """ ssss """ n """ ';
			         formattedCode: '1 max: 2; "e"min: 3';
			         value: 1).
		        (self new
			         source: '"a" #( "b" 1 "c" two "d" ( "e" 3 "f" ) "g" ) "h"';
			         nodeAt: 'DDD 000EEE090FFF0AAA0GGG0BBBBBBCBBBBBB0JJJ00 KKK';
			         styled: '""" 00 """ n """ ### """ 1 """ n """ 1 """ 0 """';
			         formattedCode: '#( 1 two #( 3 ) )';
			         value: #( 1 #two #( 3 ) )).
		        (self new
			         source: '"a" [ "b" 1 "c" . "d" 2 "e" ] "f"';
			         nodeAt: 'DDD 00EEE0548884449994A0CCC00 FFF';
			         styled: '""" 0 """ n """ . """ n """ 0 """';
			         formattedCode: '[ "a""b""f" 1. "c" "d" 2 "e" ]';
			         value: 1).
		        (self new
			         source: '"a" [ "b" : "c" x "d" : "e" y "f" | "g" ] "h"';
			         nodeAt: '888 00999000AAA030BBB000FFF0C0GGG000III00 JJJ';
			         styled: '""" 0 """ : """ B """ : """ B """ | """ 0 """';
			         formattedCode: '[ :x "a""b""c""d" :y "e""f" | "g""h" ]').
		        (self new
			         source: '"a" [ "b" | "c" x "d" y "e" | "f" ] "g"';
			         nodeAt: 'FFF 00GGG044999464AAA4B0DDD000EEE00 HHH';
			         styled: '""" 0 """ | """ T """ T """ | """ 0 """';
			         formattedCode: '[ "a""b""g" | x "c" "d" y "e" | "f" ]';
			         notices:
				         #( #( 17 17 17 'Unused variable' )
				            #( 23 23 23 'Unused variable' ) )).
		        (self new
			         source: '"a" #[ "b" 1 "c" 2 "d" ] "e"';
			         nodeAt: '888 000999060AAA070BBB00 CCC';
			         styled: '""" 00 """ n """ n """ 0 """';
			         formattedCode: '#[ 1 2 ]';
			         value: #[ 1 2 ]).
		        (self new
			         source: '"a" { "b" 1 "c" . "d" 2 "e" } "f"';
			         nodeAt: 'CCC 00DDD040777000888090BBB00 EEE';
			         styled: '""" 0 """ n """   """ n """ 0 """';
			         formattedCode: '{ 1. 2 }';
			         value: #( 1 2 )).
		        (self new
			         source: ' "a" | "b" x "c" | "d" x "e" := "f" 5 "g" ';
			         nodeAt: ' 666 00777020888000III0F9HHH9999DDD9A EEE ';
			         styled: ' """ | """ T """ | """ t """    """ n """ ';
			         formattedCode: '| x "a" "b" "c" | "d" x := 5 "e" "f" "g"';
			         value: 5).
		        (self new
			         source: ' "a" ^ "b" 5 "c" ';
			         nodeAt: ' 555 0066601 777 ';
			         styled: ' """ ^ """ n """ ';
			         formattedCode: '^ 5';
			         value: 5).
		        (self new
			         source: ' "a" foo: "b" x "c" bar: "d" y "e" ^ "f" x "g" ';
			         nodeAt: '0777000000888030999000000III0A0JJJ0CCGGGCD0HHH0';
			         styled: ' """ pppp """ A """ pppp """ A """ ^ """ a """ ';
			         formattedCode: 'foo: x bar: y "d" "e" ^ x "f" "g"';
			         isScripting: false;
			         value: 1).
		        (self new
			         source: 'foo "a" < "b" bar "c" > "d" < "e" baz "f" > "g" ';
			         nodeAt: '0000BBB08888888888888880EEE09999999999999990HHH0';
			         styled: 'ppp """ < """ <<< """ < """ < """ <<< """ < """ ';
			         formattedCode:
				         'foo "a" "b" "c" "d" "e" "f" "g" <bar> <baz> ';
			         isScripting: false).
		        (self new
			         source: 'foo "a" < "b" bar: "c" 1 "d" z: "e" 2 "f" > "g"';
			         nodeAt: '0000CCC08888888888888889888888888888A8888880III';
			         styled: 'ppp """ < """ <<<< """ n """ << """ n """ < """';
			         formattedCode:
				         'foo "a" "b" "c" "d" "e" "f" "g" <bar: 1 z: 2> ';
			         isScripting: false).

		        "Unfinished comments"
		        (self new
			         source: '"unfinished';
			         nodeAt: '00000000000';
			         styled: 'XXXXXXXXXXX';
			         isFaulty: true;
			         notices: #( #( 1 11 12 'Unmatched " in comment.' ) )).
		        (self new
			         source: '"also unfinished""';
			         nodeAt: '000000000000000000';
			         styled: 'XXXXXXXXXXXXXXXXXX';
			         isFaulty: true;
			         notices: #( #( 1 18 19 'Unmatched " in comment.' ) )).
		        (self new
			         source: '"';
			         nodeAt: '0';
			         styled: 'X';
			         isFaulty: true;
			         notices: #( #( 1 1 2 'Unmatched " in comment.' ) )).
		        (self new
			         source: '"""';
			         nodeAt: '000';
			         styled: 'XXX';
			         isFaulty: true;
			         notices: #( #( 1 3 4 'Unmatched " in comment.' ) )) }.


	"Setup default values"
	self new
		group: #badComments;
		isScripting: true;
		isFaulty: false;
		skip: #testSimpleFormattedCode;
		"The simple one is too much broken"applyDefaultTo: list.
	^ list
]

{ #category : 'accessing' }
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
			         styled: 'X n s n';
			         notices: #( #( 1 7 8 ''')'' expected' ) )).
		        (self new
			         source: '#( 1 + 2';
			         nodeAt: '00010203';
			         styled: 'XX n # n';
			         formattedCode: '#( 1 #+ 2';
			         notices: #( #( 1 8 9 ''')'' expected' ) )).
		        (self new
			         source: '[ 1 + 2';
			         nodeAt: '0032224';
			         styled: 'X n s n';
			         notices: #( #( 1 7 8 ''']'' expected' ) )).
		        (self new
			         source: '#[ 1 2';
			         nodeAt: '000102';
			         styled: 'XX n n';
			         notices: #( #( 1 6 7 ''']'' expected' ) )).
		        (self new
			         source: '{ 1 + 2';
			         nodeAt: '0021113';
			         styled: 'X n s n';
			         notices: #( #( 1 7 8 '''}'' expected' ) )).
		        (self new
			         source: '1 + 2 )';
			         nodeAt: '2111300';
			         styled: 'n s n X';
			         notices: #( #( 1 7 7 'Missing opener for closer: )' ) )).
		        (self new
			         source: '1 + 2 ]';
			         nodeAt: '2111300';
			         styled: 'n s n X';
			         notices: #( #( 1 7 7 'Missing opener for closer: ]' ) )).
		        (self new
			         source: '1 + 2 }';
			         nodeAt: '2111300';
			         styled: 'n s n X';
			         notices: #( #( 1 7 7 'Missing opener for closer: }' ) )).
		        (self new
			         source: '( ';
			         nodeAt: '00';
			         styled: 'X ';
			         notices: #( #( 3 2 3 'Variable or expression expected' )
				            #( 1 2 3 ''')'' expected' ) )).
		        (self new
			         source: '#( ';
			         nodeAt: '00 ';
			         styled: 'XX ';
			         notices: #( #( 1 2 4 ''')'' expected' ) )).
		        (self new
			         source: '[ ';
			         nodeAt: '00';
			         styled: 'X ';
			         notices: #( #( 1 2 3 ''']'' expected' ) )).
		        (self new
			         source: '#[ ';
			         nodeAt: '00 ';
			         styled: 'XX ';
			         notices: #( #( 1 2 4 ''']'' expected' ) )).
		        (self new
			         source: '{ ';
			         nodeAt: '0 ';
			         styled: 'X ';
			         notices: #( #( 1 1 3 '''}'' expected' ) )).
		        (self new
			         source: '{ [ ( ';
			         nodeAt: '001133';
			         styled: 'X X X ';
			         notices: #( #( 7 6 7 'Variable or expression expected' )
				            #( 5 6 7 ''')'' expected' )
				            #( 3 6 7 ''']'' expected' )
				            #( 1 6 7 '''}'' expected' ) )).
		        (self new
			         source: ' )';
			         nodeAt: ' 0';
			         styled: ' X';
			         notices: #( #( 2 2 2 'Missing opener for closer: )' ) )).
		        (self new
			         source: ' ]';
			         nodeAt: ' 0';
			         styled: ' X';
			         notices: #( #( 2 2 2 'Missing opener for closer: ]' ) );
			         skip: #testCodeImporter). "FIXME. code importer do not like rogue starting `]`"
		        (self new
			         source: ' }';
			         nodeAt: ' 0';
			         styled: ' X';
			         notices: #( #( 2 2 2 'Missing opener for closer: }' ) )).
		        (self new
			         source: ' ) ] }';
			         nodeAt: ' 21100';
			         styled: ' X X X';
			         notices: #( #( 2 2 2 'Missing opener for closer: )' )
				            #( 2 4 4 'Missing opener for closer: ]' )
				            #( 2 6 6 'Missing opener for closer: }' ) )).

		        "Compounds with an unexped thing inside"
		        (self new
			         source: '(1]2)';
			         nodeAt: '23154';
			         styled: 'XnXnX';
			         formattedCode: '( 1 ]. 2 )';
			         notices:
				         #( #( 1 2 3 ''')'' expected' )
				            #( 1 3 3 'Missing opener for closer: ]' )
				            #( 4 5 5 'Missing opener for closer: )' ) )).
		        (self new
			         source: '(1}2)';
			         nodeAt: '23154';
			         styled: 'XnXnX';
			         formattedCode: '( 1 }. 2 )';
			         notices:
				         #( #( 1 2 3 ''')'' expected' )
				            #( 1 3 3 'Missing opener for closer: }' )
				            #( 4 5 5 'Missing opener for closer: )' ) )).
		        (self new
			         source: '(1. 2)';
			         nodeAt: '120043';
			         styled: 'XnX nX';
			         formattedCode: '( 1. 2 )';
			         notices:
				         #( #( 1 2 3 ''')'' expected' )
				            #( 5 6 6 'Missing opener for closer: )' ) )).
		        (self new
			         source: '[1)2]';
			         nodeAt: '03240';
			         styled: '0nXn0';
			         formattedCode: '[ 1 ). 2 ]';
			         notices: #( #( 2 3 3 'Missing opener for closer: )' ) )).
		        (self new
			         source: '[1}2]';
			         nodeAt: '03240';
			         styled: '0nXn0';
			         formattedCode: '[ 1 }. 2 ]';
			         notices: #( #( 2 3 3 'Missing opener for closer: }' ) )).
		        (self new
			         source: '#(1]2}3)';
			         nodeAt: '00123450';
			         styled: '00n#n#n0';
			         formattedCode: '#( 1 #'']'' 2 #''}'' 3 )';
			         isFaulty: false;
			         value: #( 1 #']' 2 #'}' 3 );
			         notices: #( #( 4 4 4 'Use a proper symbol literal' )
				            #( 6 6 6 'Use a proper symbol literal' ) )). "`#(` can eat almost anything"
		        (self new
			         source: '#( 0 1r2 4 )';
			         nodeAt: '000102330400';
			         styled: 'XX n XX# n X';
			         formattedCode: '#( 0 1 r2 4 )';
			         notices:
				         #( #( 6 6 7 'an integer greater than 1 as valid radix expected' ) )). "Almost anything..."
		        (self new
			         source: '#( ( 0 1 2 ) )';
			         nodeAt: '00011213141100';
			         styled: '00 1 n n n 1 0';
			         formattedCode: '#( #( 0 1 2 ) )';
			         isFaulty: false;
			         value: #( #( 0 1 2 ) )).
		        (self new
			         source: '#( #( 0 1 2 ) )';
			         nodeAt: '000111213141100';
			         styled: '00 11 n n n 1 0';
			         isFaulty: false;
			         value: #( #( 0 1 2 ) )).
		        (self new
			         source: '#( #( 0 1 2 )';
			         nodeAt: '0001112131411';
			         styled: 'XX 00 n n n 0';
			         notices: #( #( 1 13 14 ''')'' expected' ) )).
		        (self new
			         source: '#( #( 0 1r2 3 ) )';
			         nodeAt: '00011121344151100';
			         styled: 'XX XX n XX# n X X';
			         formattedCode: '#( #( 0 1 r2 3 ) )';
			         notices:
				         #( #( 9 9 10 'an integer greater than 1 as valid radix expected' ) )).
		        (self new
			         source: '#( #( 0 1r2 3 )';
			         nodeAt: '000111213441511';
			         styled: 'XX XX n XX# n X';
			         formattedCode: '#( #( 0 1 r2 3 )';
			         notices:
				         #( #( 9 9 10 'an integer greater than 1 as valid radix expected' )
				            #( 1 15 16 ''')'' expected' ) )).
		        (self new
			         source: '#( #[ 0 1 2 ] )';
			         nodeAt: '000111213141100';
			         styled: '00 00 n n n 0 0';
			         isFaulty: false;
			         value: #( #[ 0 1 2 ] )).
		        (self new
			         source: '#( #[ 0 1000 2 ] )';
			         nodeAt: '000111213333141100';
			         styled: 'XX XX n XXXX n X X';
			         notices: #( #( 9 12 9 '8-bit integer expected' ) )).
		        (self new
			         source: '#( #[ 0 1 2 )';
			         nodeAt: '0001112131415';
			         styled: 'XX XX n n n X';
			         notices: #( #( 13 13 13 '8-bit integer expected' )
				            #( 4 13 14 ''']'' expected' )
				            #( 1 13 14 ''')'' expected' ) )).
		        (self new
			         source: '#( #[ 0 1 2 ]';
			         nodeAt: '0001112131411';
			         styled: 'XX 00 n n n 0';
			         notices: #( #( 1 13 14 ''')'' expected' ) )).
		        (self new
			         source: '[(#("a"("b"###("c"###["d"]#["e"]["f"]))))]';
			         nodeAt: '0222FFF9999AAAAAAABBBBBBBBCCCCCCDAAAEA9220';
			         styled: '0011"""2"""3333"""1111"""111"""1#"""#32100';
			         formattedCode:
				         '[ #( #( #( #[ ] #[ ] #''['' #'']'' ) ) ) "a" "b" "c" "d" "e" "f" ]';
			         isFaulty: false;
			         value: #( #( #( #[  ] #[  ] #'[' #']' ) ) );
			         notices: #( #( 33 33 33 'Use a proper symbol literal' )
				            #( 37 37 37 'Use a proper symbol literal' ) )). "dry [ and ] in lit array are symbols"
		        (self new
			         source: '#[ 1 ) 2 ]';
			         nodeAt: '0001020300';
			         styled: 'XX n X n X';
			         notices: #( #( 6 6 6 '8-bit integer expected' ) )).
		        (self new
			         source: '#[ 1 } 2 ]';
			         nodeAt: '0001020300';
			         styled: 'XX n X n X';
			         notices: #( #( 6 6 6 '8-bit integer expected' ) )).
		        (self new
			         source: '#[ 1 a 2 ]';
			         nodeAt: '0001020300';
			         styled: 'XX n X n X';
			         notices: #( #( 6 6 6 '8-bit integer expected' ) )).
		        (self new
			         source: '#[ 1 -1 2 ]';
			         nodeAt: '00010220300';
			         styled: 'XX n XX n X';
			         notices: #( #( 6 7 6 '8-bit integer expected' ) )).
		        (self new
			         source: '#[ 1 1.0 2 ]';
			         nodeAt: '000102220300';
			         styled: 'XX n XXX n X';
			         notices: #( #( 6 8 6 '8-bit integer expected' ) )).
		        (self new
			         source: '#[ 1 256 2 ]';
			         nodeAt: '000102220300';
			         styled: 'XX n XXX n X';
			         notices: #( #( 6 8 6 '8-bit integer expected' ) )).
		        (self new
			         source: '{1)2}';
			         nodeAt: '02130';
			         styled: '0nXn0';
			         formattedCode: '{ 1 ). 2 }';
			         notices: #( #( 2 3 3 'Missing opener for closer: )' ) )).
		        (self new
			         source: '{1]2}';
			         nodeAt: '02130';
			         styled: '0nXn0';
			         formattedCode: '{ 1 ]. 2 }';
			         notices: #( #( 2 3 3 'Missing opener for closer: ]' ) )).

		        "...or without expected thing"
		        "Note: all compounds `[]` `#()` `#[]` `{}` are legal empty, except one"
		        (self new
			         source: '()';
			         nodeAt: '00';
			         styled: '0X';
			         notices: #( #( 1 2 2 'Variable or expression expected' ) )).
		        (self new
			         source: '[ ]';
			         nodeAt: '000';
			         styled: '0 0';
			         isFaulty: false).
		        (self new
			         source: '#( )';
			         nodeAt: '0000';
			         styled: '00 0';
			         isFaulty: false;
			         value: #(  )).
		        (self new
			         source: '#[ ]';
			         nodeAt: '0000';
			         styled: '00 0';
			         isFaulty: false;
			         value: #[  ]).
		        (self new
			         source: '{ }';
			         nodeAt: '000';
			         styled: '0 0';
			         isFaulty: false;
			         value: #(  )).

		        "Bad sequence. The first expression is considered unfinished."
		        (self new
			         source: '1 2';
			         nodeAt: '213';
			         styled: 'n X';
			         formattedCode: '1. 2';
			         notices: #( #( 1 2 3 'End of statement expected' ) )).
		        (self new
			         source: '1 foo 2';
			         nodeAt: '3222214';
			         styled: 'n sss X';
			         formattedCode: '1 foo. 2';
			         notices: #( #( 1 6 7 'End of statement expected' ) )).
		        (self new
			         source: '(1)2';
			         nodeAt: '2223';
			         styled: '0n0X';
			         formattedCode: '1. 2';
			         notices: #( #( 1 3 4 'End of statement expected' ) )).
		        (self new
			         source: '1(2)';
			         nodeAt: '2333';
			         styled: 'nXn0';
			         formattedCode: '1. 2';
			         notices: #( #( 1 1 2 'End of statement expected' ) )).
		        (self new
			         source: '(1)(2)';
			         nodeAt: '222333';
			         styled: '0n0Xn0';
			         formattedCode: '1. 2';
			         notices: #( #( 1 3 4 'End of statement expected' ) )).
		        (self new
			         source: '#hello#world';
			         nodeAt: '222222333333';
			         styled: '######X#####';
			         formattedCode: '#hello. #world';
			         notices: #( #( 1 6 7 'End of statement expected' ) )).
		        (self new
			         source: '$h$w';
			         nodeAt: '2233';
			         styled: '$$X$';
			         formattedCode: '$h. $w';
			         notices: #( #( 1 2 3 'End of statement expected' ) )).
		        (self new
			         source: '[1][2]';
			         nodeAt: '242575';
			         styled: '0n0Xn0';
			         formattedCode: '[ 1 ]. [ 2 ]';
			         notices: #( #( 1 3 4 'End of statement expected' ) )).
		        (self new
			         source: '{1}{2}';
			         nodeAt: '232454';
			         styled: '0n0Xn0';
			         formattedCode: '{ 1 }. { 2 }';
			         notices: #( #( 1 3 4 'End of statement expected' ) )).
		        (self new
			         source: '#(1)#(2)';
			         nodeAt: '22324454';
			         styled: '00n0X0n0';
			         formattedCode: '#( 1 ). #( 2 )';
			         notices: #( #( 1 4 5 'End of statement expected' ) )).
		        (self new
			         source: '#[1]#[2]';
			         nodeAt: '22324454';
			         styled: '00n0X0n0';
			         formattedCode: '#[ 1 ]. #[ 2 ]';
			         notices: #( #( 1 4 5 'End of statement expected' ) )).

		        "Bad temporary variable declarations"
		        "Note: bad temporaries will be stored as error statements"
		        (self new
			         source: '| ';
			         nodeAt: '0 ';
			         styled: 'X ';
			         formattedCode: '| | ';
			         notices: #( #( 1 1 3 '''|'' or variable expected' ) )).
		        (self new
			         source: '| a b';
			         nodeAt: '00102';
			         styled: 'X X X';
			         formattedCode: '| a b | ';
			         notices: #( #( 1 5 6 '''|'' or variable expected' ) )).
		        (self new
			         source: '|- 1';
			         nodeAt: '1224';
			         styled: 'XX n';
			         formattedCode: '| | . - 1';
			         notices: #( #( 1 1 2 '''|'' or variable expected' )
				            #( 2 1 2 'Variable or expression expected' ) )). "Here |- is scanned as a single operator token, so parser must work more"
		        (self new
			         source: '| 1';
			         nodeAt: '102';
			         styled: 'X X';
			         formattedCode: '| | . 1';
			         notices: #( #( 1 1 3 '''|'' or variable expected' ) )).
		        "Note that the | character is also a binary operator, so a missing opening | become a binary call with a missing argument (see bellow)"
		        (self new
			         source: 'a | ';
			         nodeAt: '1000';
			         styled: 'u s ';
			         raise: UndeclaredVariableRead;
			         notices:
				         #( #( 1 1 1 'Undeclared variable' )
				            #( 5 4 5 'Variable or expression expected' ) )).
		        (self new
			         source: 'a || ';
			         nodeAt: '10000';
			         styled: 'u uu ';
			         raise: UndeclaredVariableRead;
			         notices:
				         #( #( 1 1 1 'Undeclared variable' )
				            #( 6 5 6 'Variable or expression expected' ) )).
		        (self new
			         source: '| | a';
			         nodeAt: '    0';
			         styled: '| | u';
			         formattedCode: 'a';
			         isParseFaulty: false;
			         isFaultyMinusUndeclared: false;
			         raise: UndeclaredVariableRead;
			         notices: #( #( 5 5 5 'Undeclared variable' ) )). "This one is legal, it is an empty list of temporaries, the | are dismissed"
		        (self new
			         source: '|| a';
			         nodeAt: '   0';
			         styled: '|| u';
			         formattedCode: 'a';
			         isParseFaulty: false;
			         isFaultyMinusUndeclared: false;
			         raise: UndeclaredVariableRead;
			         notices: #( #( 4 4 4 'Undeclared variable' ) )). "Same, but are messing with the Scanner"
		        (self new
			         source: ' ||| a';
			         nodeAt: '   002';
			         styled: ' ||X u';
			         formattedCode: ' | a';
			         notices: #( #( 4 3 4 'Variable or expression expected' )
				            #( 6 6 6 'Undeclared variable' ) )). "this one is a empty temps and a binary operator | with a mising receiver"
		        (self new
			         source: ' |||| a';
			         nodeAt: '   0002';
			         styled: ' ||Xu u';
			         formattedCode: ' || a';
			         notices: #( #( 4 3 4 'Variable or expression expected' )
				            #( 7 7 7 'Undeclared variable' ) )). "this one is a empty temps and a binary operator || with a mising receiver"
		        (self new
			         source: '| a | | a';
			         nodeAt: '001000224';
			         styled: '| T | X t';
			         notices: #( #( 7 6 7 'Variable or expression expected' )
				            #( 9 9 9 'Unitialized variable' ) )). "A valid temporary followed by a binary operator with a missing receiver"
		        (self new
			         source: '| a ||a';
			         nodeAt: '0010024';
			         styled: '| T |Xt';
			         formattedCode: '| a | | a';
			         notices: #( #( 6 5 6 'Variable or expression expected' )
				            #( 7 7 7 'Unitialized variable' ) )). "same"

		        "Unexpected parameters (or columns)"
		        "Note that `:a` is not a token but a special `:` followed by an identifier, whereas `a:` is a single token."
		        "Nevertheless, the parser will try to catch unexpected :a together"
		        (self new
			         source: ':a';
			         nodeAt: '00';
			         styled: 'XX';
			         notices: #( #( 1 2 1 'Unexpected block parameter' ) )).
		        (self new
			         source: '::a';
			         nodeAt: '122';
			         styled: 'XXX';
			         formattedCode: ':. :a';
			         notices:
				         #( #( 1 1 1 'Unexpected token' )
				            #( 2 3 2 'Unexpected block parameter' ) )).
		        (self new
			         source: ':::a';
			         nodeAt: '1233';
			         styled: 'XXXX';
			         formattedCode: ':. :. :a';
			         notices:
				         #( #( 1 1 1 'Unexpected token' )
				            #( 2 2 2 'Unexpected token' )
				            #( 3 4 3 'Unexpected block parameter' ) )).
		        (self new
			         source: '::';
			         nodeAt: '12';
			         styled: 'XX';
			         formattedCode: ':. :';
			         notices:
				         #( #( 1 1 1 'Unexpected token' )
				            #( 2 2 2 'Unexpected token' ) )).
		        (self new
			         source: ':a foo';
			         nodeAt: '110000';
			         styled: 'XX sss';
			         notices: #( #( 1 2 1 'Unexpected block parameter' ) )).
		        (self new
			         source: 'a :foo';
			         nodeAt: '213333';
			         styled: 'u XXXX';
			         formattedCode: 'a. :foo';
			         notices:
				         #( #( 1 1 1 'Undeclared variable' )
				            #( 1 2 3 'End of statement expected' )
				            #( 3 6 3 'Unexpected block parameter' ) )).
		        (self new
			         source: 'a : foo';
			         nodeAt: '2133333';
			         styled: 'u XXXXX';
			         formattedCode: 'a. :foo';
			         notices:
				         #( #( 1 1 1 'Undeclared variable' )
				            #( 1 2 3 'End of statement expected' )
				            #( 3 7 3 'Unexpected block parameter' ) )).
		        (self new
			         source: 'a:';
			         nodeAt: '00';
			         styled: 'Xs';
			         formattedCode: ' a: ';
			         notices: #( #( 1 0 1 'Variable or expression expected' )
				            #( 3 2 3 'Variable or expression expected' ) )). "keyword message with a missing receiver and argument"
		        (self new
			         source: 'a::';
			         nodeAt: '225';
			         styled: 'XsX';
			         formattedCode: ' a: . :';
			         notices: #( #( 1 0 1 'Variable or expression expected' )
				            #( 3 2 3 'Variable or expression expected' )
				            #( 1 2 3 'End of statement expected' )
				            #( 3 3 3 'Unexpected token' ) )). "same plus an unexpected :"
		        (self new
			         source: 'a:foo';
			         nodeAt: '00222';
			         styled: 'Xsuuu';
			         formattedCode: ' a: foo';
			         notices: #( #( 1 0 1 'Variable or expression expected' )
				            #( 3 5 3 'Undeclared variable' ) )). "keyword message with a missing receiver"
		        (self new
			         source: 'a::foo';
			         nodeAt: '002222';
			         styled: 'XsXXXX';
			         formattedCode: ' a: :foo';
			         notices: #( #( 1 0 1 'Variable or expression expected' )
				            #( 3 6 3 'Unexpected block parameter' ) )).
		        (self new
			         source: ':a:foo';
			         nodeAt: '122444';
			         styled: 'XXsuuu';
			         formattedCode: ':. a: foo';
			         notices:
				         #( #( 1 1 1 'Unexpected token' )
				            #( 2 1 2 'Variable or expression expected' )
				            #( 4 6 4 'Undeclared variable' ) )).
		        (self new
			         source: '|:a|';
			         nodeAt: '1332';
			         styled: 'XXXs';
			         formattedCode: '| | . :a | ';
			         notices: #( #( 1 1 2 '''|'' or variable expected' )
				            #( 2 3 2 'Unexpected block parameter' )
				            #( 5 4 5 'Variable or expression expected' ) )).
		        (self new
			         source: '|:a';
			         nodeAt: '122';
			         styled: 'XXX';
			         formattedCode: '| | . :a';
			         notices: #( #( 1 1 2 '''|'' or variable expected' )
				            #( 2 3 2 'Unexpected block parameter' ) )).
		        (self new
			         source: '|::a';
			         nodeAt: '1233';
			         styled: 'XXXX';
			         formattedCode: '| | . :. :a';
			         notices: #( #( 1 1 2 '''|'' or variable expected' )
				            #( 2 2 2 'Unexpected token' )
				            #( 3 4 3 'Unexpected block parameter' ) )).
		        (self new
			         source: '|a:|';
			         nodeAt: '1224';
			         styled: 'XXsX';
			         formattedCode: '| | . a: | ';
			         notices: #( #( 1 1 2 '''|'' or variable expected' )
				            #( 2 1 2 'Variable or expression expected' )
				            #( 4 3 4 'Variable or expression expected' )
				            #( 5 4 5 'Variable or expression expected' ) )).
		        (self new
			         source: '|a:';
			         nodeAt: '122';
			         styled: 'XXs';
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
			         styled: '0:B X0';
			         formattedCode: '[ :a | b ]';
			         raise: UndeclaredVariableRead;
			         notices: #( #( 5 4 5 '''|'' or parameter expected' )
				            #( 5 5 5 'Undeclared variable' ) )). "FIXME"
		        (self new
			         source: '[:a 1]';
			         nodeAt: '001040';
			         styled: '0:B X0';
			         formattedCode: '[ :a | 1 ]';
			         value: 1;
			         notices: #( #( 5 4 5 '''|'' or parameter expected' ) )). "FIXME"
		        (self new
			         source: '[:a :]';
			         nodeAt: '001000';
			         styled: '0:B :X';
			         formattedCode: '[ :a : | ]';
			         value: nil;
			         notices: #( #( 6 5 6 'Variable name expected' ) )). "FIXME"
		        (self new
			         source: '[:a ::b]';
			         nodeAt: '00100220';
			         styled: '0:B :XX0';
			         formattedCode: '[ :a ::b | ]';
			         value: nil;
			         notices: #( #( 6 7 6 'Variable name expected' ) )). "FIXME"
		        (self new
			         source: '[:a :b]';
			         nodeAt: '0010020';
			         styled: '0:B :B0';
			         formattedCode: '[ :a :b | ]';
			         isFaulty: false). "no pipe (and no body) is legal"
		        (self new
			         source: '[: a : b]';
			         nodeAt: '000100020';
			         styled: '0: B : B0';
			         formattedCode: '[ :a :b | ]';
			         isFaulty: false). "spaces are also legal"
		        (self new
			         source: '[:a:b]';
			         nodeAt: '004460';
			         styled: '0:Xsu0';
			         formattedCode: '[ : | a: b ]';
			         notices: #( #( 3 2 3 'Variable name expected' )
				            #( 3 2 3 '''|'' or parameter expected' )
				            #( 3 2 3 'Name already defined' )
				            #( 3 2 3 'Variable or expression expected' )
				            #( 5 5 5 'Undeclared variable' ) )). "FIXME?"
		        (self new
			         source: '[ a: ]';
			         nodeAt: '002220';
			         styled: '0 Xs X';
			         notices: #( #( 3 2 3 'Variable or expression expected' )
				            #( 6 5 6 'Variable or expression expected' ) )). "no parameters, a keyword message send witout receiver nor arguments"
		        (self new
			         source: '[ | ]';
			         nodeAt: '00200';
			         styled: '0 X X';
			         formattedCode: '[ | | ]';
			         notices: #( #( 3 3 5 '''|'' or variable expected' ) )).
		        (self new
			         source: '[ | b ]';
			         nodeAt: '0022300';
			         styled: '0 X X X';
			         formattedCode: '[ | b | ]';
			         notices: #( #( 3 5 7 '''|'' or variable expected' ) )).
		        (self new
			         source: '[ :a | | a b ]';
			         nodeAt: '00010003343500';
			         styled: '0 :B | X b X X';
			         formattedCode: '[ :a | | a b | ]';
			         notices: #( #( 8 12 14 '''|'' or variable expected' ) )).
		        (self new
			         source: '[ :a || a b ]';
			         nodeAt: '0001003343500';
			         styled: '0 :B |X b X X';
			         formattedCode: '[ :a | | a b | ]';
			         notices: #( #( 7 11 13 '''|'' or variable expected' ) )).
		        (self new
			         source: '[:a| | |b]';
			         nodeAt: '0010022230';
			         styled: '0:B| | |u0';
			         formattedCode: '[ :a | b ]';
			         isParseFaulty: false;
			         isFaultyMinusUndeclared: false;
			         raise: UndeclaredVariableRead;
			         notices: #( #( 9 9 9 'Undeclared variable' ) )). "Explicit empty list of temporaries"
		        (self new
			         source: '[:a| ||a]';
			         nodeAt: '001002230';
			         styled: '0:B| ||b0';
			         formattedCode: '[ :a | a ]';
			         isFaulty: false;
			         value: 1). "Same but mess with the Scanner"
		        (self new
			         source: '[:a|| |a]';
			         nodeAt: '001022230';
			         styled: '0:B|| |b0';
			         formattedCode: '[ :a | a ]';
			         isFaulty: false;
			         value: 1). "Same"
		        (self new
			         source: '[:a|||a]';
			         nodeAt: '00102230';
			         styled: '0:B|||b0';
			         formattedCode: '[ :a | a ]';
			         isFaulty: false;
			         value: 1). "Same"
		        (self new
			         source: '[:a||||a]';
			         nodeAt: '001022350';
			         styled: '0:B|||Xb0';
			         formattedCode: '[ :a | | a ]';
			         notices: #( #( 7 6 7 'Variable or expression expected' ) )). "same + binary | without receiver"

		        "Unclosed blocks"
		        (self new
			         source: '[ : | ';
			         nodeAt: '000000';
			         styled: 'X     ';
			         notices: #( #( 1 6 7 ''']'' expected' ) )).
		        (self new
			         source: '[:';
			         nodeAt: '00';
			         styled: 'X ';
			         formattedCode: '[ : | ';
			         notices: #( #( 1 2 3 ''']'' expected' ) )).
		        (self new
			         source: '[ :a :b | ';
			         nodeAt: '0000000000';
			         styled: 'X         ';
			         notices: #( #( 1 10 11 ''']'' expected' ) )).
		        (self new
			         source: '[ :a :b';
			         nodeAt: '0000000';
			         styled: 'X      ';
			         formattedCode: '[ :a :b | ';
			         notices: #( #( 1 7 8 ''']'' expected' ) )).
		        (self new
			         source: '[ :a a';
			         nodeAt: '000002';
			         styled: 'X    X';
			         formattedCode: '[ :a | a';
			         notices: #( #( 1 6 7 ''']'' expected' ) )).
		        (self new
			         source: '[ :a b';
			         nodeAt: '000002';
			         styled: 'X    u';
			         formattedCode: '[ :a | b';
			         notices:
				         #( #( 6 6 6 'Undeclared variable' )
				            #( 1 6 7 ''']'' expected' ) )).
		        (self new
			         source: '[ :a | ';
			         nodeAt: '0000000';
			         styled: 'X      ';
			         notices: #( #( 1 7 8 ''']'' expected' ) )).
		        (self new
			         source: '[ :a | a';
			         nodeAt: '00000002';
			         styled: 'X      X';
			         notices: #( #( 1 8 9 ''']'' expected' ) )).
		        (self new
			         source: '[ :a | b';
			         nodeAt: '00000002';
			         styled: 'X      u';
			         notices:
				         #( #( 8 8 8 'Undeclared variable' )
				            #( 1 8 9 ''']'' expected' ) )).
		        (self new
			         source: '[ | ';
			         nodeAt: '0020';
			         styled: 'X X ';
			         formattedCode: '[ | | ';
			         notices: #( #( 3 3 5 '''|'' or variable expected' )
				            #( 1 4 5 ''']'' expected' ) )).
		        (self new
			         source: '[ | 1';
			         nodeAt: '00213';
			         styled: 'X X X';
			         formattedCode: '[ | | . 1';
			         notices: #( #( 3 3 5 '''|'' or variable expected' )
				            #( 1 5 6 ''']'' expected' ) )).
		        (self new
			         source: '[ | a b';
			         nodeAt: '0022324';
			         styled: 'X X X X';
			         formattedCode: '[ | a b | ';
			         notices: #( #( 3 7 8 '''|'' or variable expected' )
				            #( 1 7 8 ''']'' expected' ) )) }.

	"Setup default values"
	self new
		group: #badExpressions;
		isScripting: true;
		isFaulty: true;
		applyDefaultTo: list.
	^ list
]

{ #category : 'accessing' }
RBCodeSnippet class >> badMethods [

	<script: 'self styleWithError: self badMethods'>
	| list |
	list := {
		        (self new
			         source: ' ';
			         nodeAt: '0';
			         styled: ' ';
			         notices: #( #( 2 1 2 'Message pattern expected' ) )).

		        "Wrong token for pattern"
		        "An empty pattern will be set, and the first statement will be a error node."
		        (self new
			         source: '5';
			         nodeAt: '3';
			         styled: 'X';
			         formattedCode: ' . 5';
			         notices: #( #( 1 0 1 'Message pattern expected' ) )).
		        (self new
			         source: '''hello''';
			         nodeAt: '3333333';
			         styled: 'X''''''''''''';
			         formattedCode: ' . ''hello''';
			         notices: #( #( 1 0 1 'Message pattern expected' ) )).
		        (self new
			         source: '#hello';
			         nodeAt: '333333';
			         styled: 'X#####';
			         formattedCode: ' . #hello';
			         notices: #( #( 1 0 1 'Message pattern expected' ) )).
		        (self new
			         source: ':';
			         nodeAt: '3';
			         styled: 'X';
			         formattedCode: ' . :';
			         notices: #( #( 1 0 1 'Message pattern expected' )
				            #( 1 1 1 'Unexpected token' ) )).
		        (self new
			         source: '#(foo bar)';
			         nodeAt: '3344435553';
			         styled: 'X0### ###0';
			         formattedCode: ' . #( foo bar )';
			         notices: #( #( 1 0 1 'Message pattern expected' ) )).

		        "Random bad token"
		        "Tought: is complainng about the bad token really better than complaining about the bad pattern?"
		        (self new
			         source: ' $';
			         nodeAt: '02';
			         styled: ' X';
			         notices: #( #( 2 2 3 'Character expected' ) )).
		        (self new
			         source: ' ''hello';
			         nodeAt: '0222222';
			         styled: ' XXXXXX';
			         notices: #( #( 2 7 8 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: ' 2r3';
			         nodeAt: '0223';
			         styled: ' XXX';
			         formattedCode: ' 2r. 3';
			         notices:
				         #( #( 2 3 4 'a digit between 0 and 1 expected' ) )).

		        "Bad aruments"
		        "The missing argument will be an error, the remainer starts the body"
		        (self new
			         source: '+ ';
			         nodeAt: '00';
			         styled: 'p ';
			         value: nil;
			         notices: #( #( 3 2 3 'Variable name expected' ) )).
		        (self new
			         source: '+ 1';
			         nodeAt: '003';
			         styled: 'p X';
			         value: nil;
			         notices: #( #( 3 2 3 'Variable name expected' ) )).
		        (self new
			         source: '+ foo: ';
			         nodeAt: '0033333';
			         styled: 'p Xsss ';
			         notices: #( #( 3 2 3 'Variable name expected' )
				            #( 3 2 3 'Variable or expression expected' )
				            #( 8 7 8 'Variable or expression expected' ) )).
		        (self new
			         source: 'foo: ';
			         nodeAt: '00000';
			         styled: 'pppp ';
			         value: nil;
			         notices: #( #( 6 5 6 'Variable name expected' ) )).
		        (self new
			         source: 'foo: 1';
			         nodeAt: '000003';
			         styled: 'pppp X';
			         value: nil;
			         notices: #( #( 6 5 6 'Variable name expected' ) )).
		        (self new
			         source: 'foo: + ';
			         nodeAt: '0000033';
			         styled: 'pppp X ';
			         notices: #( #( 6 5 6 'Variable name expected' )
				            #( 6 5 6 'Variable or expression expected' )
				            #( 8 7 8 'Variable or expression expected' ) )).
		        (self new
			         source: 'foo: bar: ';
			         nodeAt: '0000000000';
			         styled: 'pppp Xppp ';
			         value: nil;
			         notices: #( #( 6 5 6 'Variable name expected' )
				            #( 11 10 11 'Variable name expected' )
				            #( 11 10 11 'Name already defined' ) )).
		        (self new
			         source: 'foo:bar:';
			         nodeAt: '00000000';
			         styled: 'ppppXppp';
			         formattedCode: 'foo: bar: ';
			         value: nil;
			         notices: #( #( 5 4 5 'Variable name expected' )
				            #( 9 8 9 'Variable name expected' )
				            #( 9 8 9 'Name already defined' ) )). "same. `foo:bar:` is no more a single token"

		        "Bad pragma message"
		        (self new
			         source: 'foo < ';
			         nodeAt: '000011';
			         styled: 'ppp X ';
			         value: nil;
			         notices: #( #( 7 6 7 'Message pattern expected' )
				            #( 5 6 7 '''>'' expected' ) )).
		        (self new
			         source: 'foo <> ';
			         nodeAt: '0000222';
			         styled: 'ppp Xu ';
			         notices: #( #( 5 4 5 'Variable or expression expected' )
				            #( 8 7 8 'Variable or expression expected' ) )).
		        (self new
			         source: 'foo < 4';
			         nodeAt: '0000114';
			         styled: 'ppp X X';
			         value: nil;
			         notices: #( #( 7 6 7 'Message pattern expected' )
				            #( 5 6 7 '''>'' expected' ) )).
		        (self new
			         source: 'foo < bar ';
			         nodeAt: '0000222222';
			         styled: 'ppp < <<<<';
			         value: nil;
			         notices: #( #( 5 10 11 '''>'' expected' ) )).
		        (self new
			         source: 'foo < bar: ';
			         nodeAt: '00002222222';
			         styled: 'ppp < <<<<<';
			         value: nil;
			         notices: #( #( 12 11 12 'Literal constant expected' )
				            #( 5 11 12 '''>'' expected' ) )).
		        (self new
			         source: 'foo < bar: 1 1 > ';
			         nodeAt: '00002222222326555';
			         styled: 'ppp < <<<< n<X s ';
			         notices:
				         #( #( 5 13 14 '''>'' expected' )
				            #( 18 17 18 'Variable or expression expected' ) )).
		        (self new
			         source: 'foo < bar ; baz > ';
			         nodeAt: '0000222222888884AA';
			         styled: 'ppp < <<<<X uuu X ';
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
			         styled: 'ppp <<<<< X ';
			         value: nil;
			         notices: #( #( 11 10 11 'Literal constant expected' ) )).
		        (self new
			         source: 'foo <bar:(1)>';
			         nodeAt: '0000222226665';
			         styled: 'ppp <<<<<Xn0s';
			         formattedCode: 'foo < bar: 1 > ';
			         notices: #( #( 10 9 10 'Literal constant expected' )
				            #( 5 9 10 '''>'' expected' )
				            #( 14 13 14 'Variable or expression expected' ) )). "FIXME. dont eat parentheses"
		        (self new
			         source: 'foo < bar: baz > ';
			         nodeAt: '00002222222666555';
			         styled: 'ppp < <<<<<Xuu s ';
			         raise: UndeclaredVariableRead;
			         notices: #( #( 12 11 12 'Literal constant expected' )
				            #( 5 11 12 '''>'' expected' )
				            #( 12 14 12 'Undeclared variable' )
				            #( 18 17 18 'Variable or expression expected' ) )).
		        (self new
			         source: 'foo < bar: 1 + 1 > ';
			         nodeAt: '0000222222232668555';
			         styled: 'ppp < <<<< n<X n s ';
			         notices:
				         #( #( 5 13 14 '''>'' expected' )
				            #( 14 13 14 'Variable or expression expected' )
				            #( 20 19 20 'Variable or expression expected' ) )).
		        (self new
			         source: 'foo < bar: [ 1 ] > ';
			         nodeAt: '0000222222266866555';
			         styled: 'ppp < <<<<<X n 0 s ';
			         notices: #( #( 12 11 12 'Literal constant expected' )
				            #( 5 11 12 '''>'' expected' )
				            #( 20 19 20 'Variable or expression expected' ) )).
		        (self new
			         source: 'foo < bar: { 1 } > ';
			         nodeAt: '0000222222266766555';
			         styled: 'ppp < <<<<<X n 0 s ';
			         notices: #( #( 12 11 12 'Literal constant expected' )
				            #( 5 11 12 '''>'' expected' )
				            #( 20 19 20 'Variable or expression expected' ) )).
		        (self new
			         source: 'foo <bar: #[ -1 ]> ';
			         nodeAt: '0000111111222332210';
			         styled: 'ppp <<<<< XX XX X< ';
			         value: nil;
			         notices: #( #( 14 15 14 '8-bit integer expected' ) )). "Literal bytes arrays are acceptable, but this one is faulty"
		        (self new
			         source: 'foo < + 1> ';
			         nodeAt: '00001111210';
			         styled: 'ppp < < n< ';
			         isFaulty: false). "Binary message is legal pragma"
		        (self new
			         source: 'foo < + > ';
			         nodeAt: '0000111110';
			         styled: 'ppp < < X ';
			         value: nil;
			         notices: #( #( 9 8 9 'Literal constant expected' ) )) }.
	"Setup default values"
	self new
		group: #badMethods;
		isScripting: false;
		isFaulty: true;
		applyDefaultTo: list.
	^ list
]

{ #category : 'accessing' }
RBCodeSnippet class >> badPositions [
	"This list focuses on node positions (and later highlighing)"

	<script:
	'self styleWithError: self badPositions. self updateSnippets: self badPositions'>
	| list |
	list := {
		        (self new
			         source: ' foo | tmp | tmp := 1 . ^ tmp . ';
			         nodeAt: '00000112221115553333411166777110';
			         styled: ' ppp | TTT | ttt    n . ^ ttt . ';
			         formattedCode: 'foo | tmp | tmp := 1. ^ tmp';
			         isScripting: false;
			         value: 1).
		        (self new
			         source: ' foo: arg bar: arr ^ arg + arr . ';
			         nodeAt: '000000111000000222044666555777330';
			         styled: ' pppp AAA pppp AAA ^ aaa s aaa . ';
			         formattedCode: 'foo: arg bar: arr ^ arg + arr';
			         isScripting: false;
			         value: 3).
		        (self new
			         source: ' | tmp | tmp := 1 . ^ tmp . ';
			         nodeAt: ' 00111000444222230005566600 ';
			         styled: ' | TTT | ttt    n . ^ ttt . ';
			         formattedCode: '| tmp | tmp := 1. ^ tmp';
			         value: 1).
		        (self new
			         source: ' foo: arg ^ arg min: arg + 2 ; abs . ';
			         nodeAt: '000000111033BBB5555558887779AAAAAA220';
			         styled: ' pppp AAA ^ aaa ssss aaa s n ; sss . ';
			         formattedCode: 'foo: arg ^ arg min: arg + 2; abs';
			         isScripting: false;
			         value: 1).
		        (self new
			         source: ' foo: arg ^ ( ( ( ( arg ) ) + ( ( 1 ) ) ) ) . ';
			         nodeAt: '0000001110334444555555555554446666666664444220';
			         styled: ' pppp AAA ^ 0 1 2 3 aaa 3 2 s 2 3 n 3 2 1 0 . ';
			         formattedCode: 'foo: arg ^ arg + 1';
			         isScripting: false;
			         value: 2).
		        (self new
			         source: ' ( [ :aaa : bbb | | ccc ddd | aaa . ] ) . ';
			         nodeAt: ' 00000111000222000334443555333666330000   ';
			         styled: ' 0 0 :BBB : BBB | | TTT TTT | bbb . 0 0 . ';
			         formattedCode: '[ :aaa :bbb | | ccc ddd | aaa ]';
			         value: 1;
			         notices:
				         #( #( 21 23 21 'Unused variable' )
				            #( 25 27 25 'Unused variable' ) )) }.

	"Setup default values"
	self new
		group: #badPositions;
		isScripting: true;
		isFaulty: false;
		applyDefaultTo: list.
	^ list
]

{ #category : 'accessing' }
RBCodeSnippet class >> badScanner [
	"While simple, the Pharo syntax has some hacks and exceptions when dealing with the scanning of token.
	Raw scanned lexical tokens are not always used as is and might be transformed or split according to specific grammatical rules."

	<script: 'self styleWithError: self badScanner'>
	| list |
	list := {
		        (self new
			         source: '1 true false nil self super thisContext';
			         nodeAt: '655555444444333322222111111000000000000';
			         styled: 'n uuuu uuuuu uuu uuuu uuuuu uuuuuuuuuuu';
			         messageNotUnderstood: #true). "Simili-keywords can be used as unary messages"
		        (self new
			         source:
				         '1"A"true"B"false"C"nil"D"self"E"super"F"thisContext"G"';
			         nodeAt:
				         '9BBB888877777777666666555555544444444DDD00000000000EEE';
			         styled:
				         'n"""uuuu   uuuuu   uuu   uuuu"""uuuuu"""uuuuuuuuuuu"""';
			         formattedCode: '1 true false nil self super thisContext';
			         messageNotUnderstood: #true). "Check if comments are lost"

		        "`-` appear in negative number and in binary operators"
		        (self new
			         source: '1-1 - 1abs-1 - 1max:-1';
			         nodeAt: '546333877729111A0000BB';
			         styled: 'nsn s nssssn s nssssnn';
			         formattedCode: '1 - 1 - 1 abs - 1 - 1 max: -1';
			         value: -1).
		        (self new
			         source: '1-1"A" - 1abs-1"B" - 1max:-1"C"';
			         nodeAt: '546888333A9992BDDD111E0000FFHHH';
			         styled: 'nsn""" s nssssn""" s nssssnn"""';
			         formattedCode: '1 - 1 - 1 abs - 1 - 1 max: -1';
			         value: -1). "Check if comments are lost"
		        (self new
			         source: '1*-1 + 1--1 + 1- -1';
			         nodeAt: '54463337228111900AA';
			         styled: 'nuun s nssn s ns nn';
			         formattedCode: '1 *- 1 + 1 -- 1 + 1 - -1';
			         messageNotUnderstood: #'*-'). "Note: long binary operators ending in `-` are not split into negative numbers"
		        (self new
			         source: '+1. -a. - 1. -"A"1';
			         nodeAt: '1300460077900AEEEC';
			         styled: 'Xn. Xu. X n. X"""n';
			         formattedCode: ' + 1. - a. - 1. - 1 "A"';
			         isFaulty: true;
			         notices: #( #( 1 0 1 'Variable or expression expected' )
				            #( 5 4 5 'Variable or expression expected' )
				            #( 6 6 6 'Undeclared variable' )
				            #( 9 8 9 'Variable or expression expected' )
				            #( 14 13 14 'Variable or expression expected' ) )). "prefix - is limited to raw integer tokens"

		        "Tokens in literal arrays are really weird."
		        (self new
			         source: '#(aa:bb:cc#aa:bb:cc#cc==#== =#=)';
			         nodeAt: '00111111112222222223334455506770';
			         styled: '00######################### ###0';
			         formattedCode:
				         '#( aa:bb:cc #''aa:bb:cc'' #cc #''=='' #''=='' #= #= )';
			         value: #( #'aa:bb:cc' #'aa:bb:cc' #cc #'==' #'==' #= #= )). "Identifiers, keywords and binary operators are implicitely transformed into symbols. Note that formatting is inconsistent."
		        (self new
			         source: '#(aa:bb:"A"cc"B"#aa:bb:cc"C"#cc"D"++"E"#++"F")';
			         nodeAt: '00777777DDD88EEE999999999FFFAAAGGGBBHHHCCCIII0';
			         styled: '00######"""##"""#########"""###"""##"""###"""0';
			         formattedCode:
				         '#( aa:bb: cc #''aa:bb:cc'' #cc #''++'' #''++'' )';
			         value: #( #aa:bb: #cc #'aa:bb:cc' #cc #'++' #'++' )). "Check if comments are lost"
		        (self new
			         source: '#(().:;[]{}^#a)';
			         nodeAt: '001123456789AA0';
			         styled: '0011##########0';
			         formattedCode:
				         '#( #( ) #''.'' #'':'' #'';'' #''['' #'']'' #''{'' #''}'' #''^'' #a )';
			         value:
				         #( #(  ) #'.' #':' #';' #'[' #']' #'{' #'}' #'^' #a );
			         notices: #( #( 5 5 5 'Use a proper symbol literal' )
				            #( 6 6 6 'Use a proper symbol literal' )
				            #( 7 7 7 'Use a proper symbol literal' )
				            #( 8 8 8 'Use a proper symbol literal' )
				            #( 9 9 9 'Use a proper symbol literal' )
				            #( 10 10 10 'Use a proper symbol literal' )
				            #( 11 11 11 'Use a proper symbol literal' )
				            #( 12 12 12 'Use a proper symbol literal' ) )). "Most special characters are transformed into single character symbols, including brackets!"
		        (self new
			         source: '#(("A")"B"."C":"D";"E"["F"]"G"{"H"}"I"^"J")';
			         nodeAt: '00BBBBBLLLCMMMDNNNEOOOFPPPGQQQHRRRISSSJTTT0';
			         styled: '001"""1"""#"""#"""#"""#"""#"""#"""#"""#"""0';
			         formattedCode:
				         '#( #( ) #''.'' #'':'' #'';'' #''['' #'']'' #''{'' #''}'' #''^'' )';
			         value: #( #(  ) #'.' #':' #';' #'[' #']' #'{' #'}' #'^' );
			         notices: #( #( 11 11 11 'Use a proper symbol literal' )
				            #( 15 15 15 'Use a proper symbol literal' )
				            #( 19 19 19 'Use a proper symbol literal' )
				            #( 23 23 23 'Use a proper symbol literal' )
				            #( 27 27 27 'Use a proper symbol literal' )
				            #( 31 31 31 'Use a proper symbol literal' )
				            #( 35 35 35 'Use a proper symbol literal' )
				            #( 39 39 39 'Use a proper symbol literal' ) )). "Check if comments are lost"
		        (self new
			         source: '#(# ## #ab #10 #. 10)';
			         nodeAt: '001022033304550670880';
			         styled: 'XXXXXXX### XXn XX nnX';
			         formattedCode: '#( # ## #ab # 10 # #''.'' 10 )';
			         isFaulty: true;
			         notices:
				         #( #( 3 3 4 'Literal expected' )
				            #( 5 6 7 'Literal expected' )
				            #( 12 12 13 'Literal expected' )
				            #( 16 16 17 'Literal expected' )
				            #( 17 17 17 'Use a proper symbol literal' ) )). "Lonely # are errors"
		        (self new
			         source: '#(#"A"##"B")';
			         nodeAt: '003555446660';
			         styled: 'XXXX""XXX""X';
			         formattedCode: '#( # ## )';
			         isFaulty: true;
			         notices:
				         #( #( 3 3 4 'Literal expected' )
				            #( 7 8 9 'Literal expected' ) )). "Check if comments are lost"
		        (self new
			         source: '#(:=aa:=:==bb:==#cc:==)';
			         nodeAt: '00112223445666778888990';
			         styled: '00####################0';
			         formattedCode:
				         '#( #'':='' aa: #= #'':='' #= bb: #''=='' #cc: #''=='' )';
			         value: #( #':=' #aa: #= #':=' #= #bb: #'==' #cc: #'==' );
			         notices: #( #( 3 4 3 'Use a proper symbol literal' )
				            #( 9 10 9 'Use a proper symbol literal' ) )). "Assigments are split, creating symbols"
		        (self new
			         source: '#(:="A"aa:="B":=="C"cc:=="D")';
			         nodeAt: '0055CCC6667DDD889EEEAAABBFFF0';
			         styled: '00##"""####"""###"""#####"""0';
			         formattedCode:
				         '#( #'':='' aa: #= #'':='' #= cc: #''=='' )';
			         value: #( #':=' #aa: #= #':=' #= #cc: #'==' );
			         notices: #( #( 3 4 3 'Use a proper symbol literal' )
				            #( 15 16 15 'Use a proper symbol literal' ) )) "Check if comments are lost" }.
	"Setup default values"
	self new
		group: #badScanner;
		isScripting: true;
		isFaulty: false;
		applyDefaultTo: list.
	^ list
]

{ #category : 'accessing' }
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
			         styled: 'u    nn. ^ u';
			         isFaulty: true;
			         isFaultyMinusUndeclared: false;
			         raise: UndeclaredVariableWrite;
			         notices:
				         #( #( 1 1 1 'Undeclared variable' )
				            #( 12 12 12 'Undeclared variable' ) )).
		        (self new
			         source: '^ a';
			         nodeAt: '001';
			         styled: '^ u';
			         isFaulty: true;
			         isFaultyMinusUndeclared: false;
			         raise: UndeclaredVariableRead;
			         notices: #( #( 3 3 3 'Undeclared variable' ) )).

		        "Uninitialized variable"
		        (self new
			         source: '| a | ^ a';
			         nodeAt: '001000223';
			         styled: '| T | ^ t';
			         notices: #( #( 9 9 9 'Unitialized variable' ) )).
		        (self new
			         source: '| a | [ a := 10 ]. ^ a';
			         nodeAt: '0010002264444552200778';
			         styled: '| T | 0 t    nn 0. ^ t').
		        (self new
			         source: '| a | [ ^ a ]. a := 10';
			         nodeAt: '0010002244522008666677';
			         styled: '| T | 0 ^ t 0. t    nn';
			         value: 10;
			         notices: #( #( 11 11 11 'Unitialized variable' ) )).

		        "Duplicated variable definition (same scope)"
		        (self new
			         source: 'foo: a bar: a ^ a';
			         nodeAt: '00000100000020445';
			         styled: 'pppp A pppp A ^ a';
			         isScripting: false;
			         value: 1;
			         notices: #( #( 13 13 13 'Name already defined' ) )).
		        (self new
			         source: '| a a | a := 10. ^ a';
			         nodeAt: '00102000533334400667';
			         styled: '| T t | t    nn. ^ t';
			         value: 10;
			         notices:
				         #( #( 3 3 3 'Unused variable' )
				            #( 5 5 5 'Name already defined' ) )).
		        (self new
			         source: '[ | a a | a := 10. a ]';
			         nodeAt: '0011213111644445511700';
			         styled: '0 | T t | t    nn. t 0';
			         value: 10;
			         notices:
				         #( #( 5 5 5 'Unused variable' )
				            #( 7 7 7 'Name already defined' ) )).
		        (self new
			         source: '[ :a :a | a ]';
			         nodeAt: '0001002000400';
			         styled: '0 :B :B | b 0';
			         value: 1;
			         notices: #( #( 7 7 7 'Name already defined' ) )).

		        "Shadowed variables"
		        (self new
			         source: 'foo: a ^ [ | a | a := 10. a ] value + a';
			         nodeAt: '00000103366778777B9999AA77C66555555444D';
			         styled: 'pppp A ^ 0 | T | t    nn. t 0 sssss s a';
			         isScripting: false;
			         value: 11;
			         notices: #( #( 14 14 14 'Name already defined' ) )).
		        (self new
			         source:
				         'foo | a | a := 1. ^ [ | a | a := 10. a ] value + a';
			         nodeAt:
				         '0000112111533334116699AABAAAECCCCDDAAF99888888777G';
			         styled:
				         'ppp | T | t    n. ^ 0 | T | t    nn. t 0 sssss s t';
			         isScripting: false;
			         value: 11;
			         notices: #( #( 25 25 25 'Name already defined' ) )).
		        (self new
			         source:
				         'foo ^ [ | a | a := 1. [ | a | a := 10. a ] value + a ] value';
			         nodeAt:
				         '0000224455655597777855CCDDEDDDHFFFFGGDDICCBBBBBBAAAJ44333333';
			         styled:
				         'ppp ^ 0 | T | t    n. 1 | T | t    nn. t 1 sssss s t 0 sssss';
			         isScripting: false;
			         value: 11;
			         notices: #( #( 27 27 27 'Name already defined' ) )).
		        (self new
			         source:
				         'foo ^ [ :a | [ | a | a := 10. a ] value + a ] value: 1';
			         nodeAt:
				         '000022444544499AABAAAECCCCDDAAF99888888777G4433333333H';
			         styled:
				         'ppp ^ 0 :B | 1 | T | t    nn. t 1 sssss s b 0 ssssss n';
			         isScripting: false;
			         value: 11;
			         notices: #( #( 18 18 18 'Name already defined' ) )).
		        (self new
			         source: 'foo: a ^ [ :a | a ] value: 10 + a';
			         nodeAt: '000001033555655585544444444AA999B';
			         styled: 'pppp A ^ 0 :B | b 0 ssssss nn s a';
			         isScripting: false;
			         value: 11;
			         notices: #( #( 13 13 13 'Name already defined' ) )).
		        (self new
			         source: 'foo | a | a := 1. ^ [ :a | a ] value: 10 + a';
			         nodeAt: '000011211153333411668889888B8877777777DDCCCE';
			         styled: 'ppp | T | t    n. ^ 0 :B | b 0 ssssss nn s t';
			         isScripting: false;
			         value: 11;
			         notices: #( #( 24 24 24 'Name already defined' ) )).
		        (self new
			         source:
				         'foo ^ [ | a | a := 1. [ :a | a ] value: 10 + a ] value';
			         nodeAt:
				         '0000224455655597777855BBBCBBBEBBAAAAAAAAGGFFFH44333333';
			         styled:
				         'ppp ^ 0 | T | t    n. 1 :B | b 1 ssssss nn s t 0 sssss';
			         isScripting: false;
			         value: 11;
			         notices: #( #( 26 26 26 'Name already defined' ) )).
		        (self new
			         source: 'foo ^ [ :a | [ :a | a ] value: 10 + a ] value: 1';
			         nodeAt: '00002244454448889888B8877777777DDCCCE4433333333F';
			         styled: 'ppp ^ 0 :B | 1 :B | b 1 ssssss nn s b 0 ssssss n';
			         isScripting: false;
			         value: 11;
			         notices: #( #( 17 17 17 'Name already defined' ) )). "phonyArgs"

		        "Write on readonly or reserved"
		        (self new
			         source: 'foo: a a := 10. ^ a';
			         nodeAt: '0000010533334422667';
			         styled: 'pppp A XXXXXXX. ^ a';
			         isScripting: false;
			         isFaulty: true;
			         notices:
				         #( #( 8 8 8 'Assignment to read-only variable' ) )).
		        (self new
			         source: '[ :a | a := 10. a ]';
			         nodeAt: '0001000533334422600';
			         styled: '0 :B | XXXXXXX. b 0';
			         isFaulty: true;
			         notices:
				         #( #( 8 8 8 'Assignment to read-only variable' ) )).
		        "The following assignment are explicitely no-op, to minimize the impact if, for some broken reasons, they are really executed"
		        (self new
			         source: 'nil := nil';
			         nodeAt: '2221333444';
			         styled: 'kkk XX kkk';
			         formattedCode: 'nil. := nil';
			         isFaulty: true;
			         isParseFaulty: true;
			         notices: #( #( 1 4 5 'End of statement expected' )
				            #( 5 10 5 'variable expected in assigment' ) )). "it is a literal, not an identifier"
		        (self new
			         source: 'true := true';
			         nodeAt: '222213334444';
			         styled: 'kkkk XX kkkk';
			         formattedCode: 'true. := true';
			         isFaulty: true;
			         isParseFaulty: true;
			         notices: #( #( 1 5 6 'End of statement expected' )
				            #( 6 12 6 'variable expected in assigment' ) )). "it is a literal, not an identifier"
		        (self new
			         source: 'false := false';
			         nodeAt: '22222133344444';
			         styled: 'kkkkk XX kkkkk';
			         formattedCode: 'false. := false';
			         isFaulty: true;
			         isParseFaulty: true;
			         notices: #( #( 1 6 7 'End of statement expected' )
				            #( 7 14 7 'variable expected in assigment' ) )). "it is a literal, not an identifier"
		        (self new
			         source: 'self := self';
			         nodeAt: '222200001111';
			         styled: 'XXXXXXXXXXXX';
			         isFaulty: true;
			         numberOfCritiques: 1;
			         notices: #( #( 1 4 1 'Assignment to read-only variable' ) )).
		        (self new
			         source: 'super := super';
			         nodeAt: '22222000011111';
			         styled: 'XXXXXXXXXXXXXX';
			         isFaulty: true;
			         numberOfCritiques: 1;
			         notices: #( #( 1 5 1 'Assignment to read-only variable' ) )).
		        (self new
			         source: 'thisContext := thisContext';
			         nodeAt: '22222222222000011111111111';
			         styled: 'XXXXXXXXXXXXXXXXXXXXXXXXXX';
			         isFaulty: true;
			         numberOfCritiques: 1;
			         notices: #( #( 1 11 1 'Assignment to read-only variable' ) )).
		        (self new
			         source: 'Object := Object';
			         nodeAt: '2222220000111111';
			         styled: 'XXXXXXXXXXXXXXXX';
			         isFaulty: true;
			         numberOfCritiques: 1;
			         notices:
				         #( #( 1 6 1 'Assignment to read-only variable' ) )).

		        "Shadowed reserved or global"
		        (self new
			         source: '| self | self := 1. ^ self';
			         nodeAt: '00111100044442222300556666';
			         styled: '| TTTT | tttt    n. ^ tttt';
			         value: 1;
			         notices: #( #( 3 6 3 'Name already defined' ) )).
		        (self new
			         source: '| super | super := 1. ^ super';
			         nodeAt: '00111110004444422223005566666';
			         styled: '| TTTTT | ttttt    n. ^ ttttt';
			         value: 1;
			         notices: #( #( 3 7 3 'Name already defined' ) )).
		        (self new
			         source: '| thisContext | thisContext := 1. ^ thisContext';
			         nodeAt: '00111111111110004444444444422223005566666666666';
			         styled: '| TTTTTTTTTTT | ttttttttttt    n. ^ ttttttttttt';
			         value: 1;
			         notices: #( #( 3 13 3 'Name already defined' ) )).
		        (self new
			         source: '| Object | Object := 1. ^ Object';
			         nodeAt: '00111111000444444222230055666666';
			         styled: '| TTTTTT | tttttt    n. ^ tttttt';
			         value: 1;
			         notices: #( #( 3 8 3 'Name already defined' ) )).
		        (self new
			         source: 'foo: self ^ self + 1';
			         nodeAt: '00000111103355554446';
			         styled: 'pppp AAAA ^ aaaa s n';
			         isScripting: false;
			         value: 2;
			         notices: #( #( 6 9 6 'Name already defined' ) )).
		        (self new
			         source: 'foo: super ^ super + 1';
			         nodeAt: '0000011111033555554446';
			         styled: 'pppp AAAAA ^ aaaaa s n';
			         isScripting: false;
			         value: 2;
			         notices: #( #( 6 10 6 'Name already defined' ) )).
		        (self new
			         source: 'foo: thisContext ^ thisContext + 1';
			         nodeAt: '0000011111111111033555555555554446';
			         styled: 'pppp AAAAAAAAAAA ^ aaaaaaaaaaa s n';
			         isScripting: false;
			         value: 2;
			         notices: #( #( 6 16 6 'Name already defined' ) )).
		        (self new
			         source: 'foo: Object ^ Object + 1';
			         nodeAt: '000001111110335555554446';
			         styled: 'pppp AAAAAA ^ aaaaaa s n';
			         isScripting: false;
			         value: 2;
			         notices: #( #( 6 11 6 'Name already defined' ) )).
		        (self new
			         source: '[ :self | self + 1 ]';
			         nodeAt: '00011110004444333500';
			         styled: '0 :BBBB | bbbb s n 0';
			         value: 2;
			         notices: #( #( 4 7 4 'Name already defined' ) )).
		        (self new
			         source: '[ :super | super + 1 ]';
			         nodeAt: '0001111100044444333500';
			         styled: '0 :BBBBB | bbbbb s n 0';
			         value: 2;
			         notices: #( #( 4 8 4 'Name already defined' ) )).
		        (self new
			         source: '[ :thisContext | thisContext + 1 ]';
			         nodeAt: '0001111111111100044444444444333500';
			         styled: '0 :BBBBBBBBBBB | bbbbbbbbbbb s n 0';
			         value: 2;
			         notices: #( #( 4 14 4 'Name already defined' ) )).
		        (self new
			         source: '[ :Object | Object + 1 ]';
			         nodeAt: '000111111000444444333500';
			         styled: '0 :BBBBBB | bbbbbb s n 0';
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
			         styled:
				         'ppp ^ 0 :BB :BB :BB :BB :BB :BB :BB :BB :BB :BBB :BBB :BBB :BBB :BBB :BBB :BBB | bb 0';
			         isScripting: false;
			         isFaulty: true;
			         notices: #( #( 7 85 7 'Too many arguments' ) )). "Too many arguments"
		        (self new
			         source:
				         'a1: a1 a2: a2 a3: a3 a4: a4 a5: a5 a6: a6 a7: a7 a8: a8 a9: a9 a10: a10 a11: a11 a12: a12 a13: a13 a14: a14 a15: a15 a16: a16 ^ a1';
			         nodeAt:
				         '00001100000220000033000004400000550000066000007700000880000099000000AAA000000BBB000000CCC000000DDD000000EEE000000FFF000000GGG0IIJJ';
			         styled:
				         'ppp AA ppp AA ppp AA ppp AA ppp AA ppp AA ppp AA ppp AA ppp AA pppp AAA pppp AAA pppp AAA pppp AAA pppp AAA pppp AAA pppp AAA ^ aa';
			         isScripting: false;
			         isFaulty: true;
			         notices: #( #( 1 130 1 'Too many arguments' ) )). "Too many arguments"
		        (self new
			         source: 'CodeError signal: ''false error''';
			         nodeAt: '1111111110000000002222222222222';
			         styled: 'ggggggggg sssssss ''''''''''''''''''''''''''';
			         raise: CodeError;
			         skip: #testEvaluateOnErrorResume). "evaluate cannot distinguishes runtime errors and compiletime errors"
		        (self new
			         source:
				         'Warning signal: ''false error''';
			         nodeAt:
				         '11111110000000002222222222222';
			         styled:
				         'ggggggg sssssss ''''''''''''''''''''''''''';
			         raise: Warning). "same, but warning are silently ignored"
		        (self new
			         source: 'RuntimeSyntaxError signal: ''false error''';
			         nodeAt: '1111111111111111110000000002222222222222';
			         styled:
				         'gggggggggggggggggg sssssss ''''''''''''''''''''''''''';
			         raise: RuntimeSyntaxError) "runtime errors are not special" }.
	"Setup default values"
	self new
		group: #badSemantic;
		isScripting: true;
		isFaulty: false;
		isParseFaulty: false;
		applyDefaultTo: list.
	^ list
]

{ #category : 'accessing' }
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
			         styled: 'n sss';
			         isFaulty: false;
			         value: 1).
		        (self new
			         source: '1 + 2';
			         nodeAt: '10002';
			         styled: 'n s n';
			         isFaulty: false;
			         value: 3).
		        (self new
			         source: '1 max: 2';
			         nodeAt: '10000002';
			         styled: 'n ssss n';
			         isFaulty: false;
			         value: 2).

		        "Missing receiver or argument in message sends.
		Note: a unary message send without a receiver will be 'correctly' mistaken as a variable, so not a parsing error"
		        "binary"
		        (self new
			         source: ' + ';
			         nodeAt: ' 00';
			         styled: ' X ';
			         notices: #( #( 2 1 2 'Variable or expression expected' )
				            #( 4 3 4 'Variable or expression expected' ) )).
		        (self new
			         source: '1 + ';
			         nodeAt: '1000';
			         styled: 'n s ';
			         notices: #( #( 5 4 5 'Variable or expression expected' ) )).
		        (self new
			         source: ' + 2';
			         nodeAt: ' 002';
			         styled: ' X n';
			         notices: #( #( 2 1 2 'Variable or expression expected' ) )).
		        "keywords"
		        (self new
			         source: ' hello: ';
			         nodeAt: ' 0000000';
			         styled: ' Xuuuuu ';
			         notices: #( #( 2 1 2 'Variable or expression expected' )
				            #( 9 8 9 'Variable or expression expected' ) )).
		        (self new
			         source: '1 hello: ';
			         nodeAt: '100000000';
			         styled: 'n uuuuuu ';
			         notices:
				         #( #( 10 9 10 'Variable or expression expected' ) )).
		        (self new
			         source: ' hello: 2';
			         nodeAt: ' 00000002';
			         styled: ' Xuuuuu n';
			         notices: #( #( 2 1 2 'Variable or expression expected' ) )).
		        (self new
			         source: ' goodby: my: ';
			         nodeAt: ' 000000000000';
			         styled: ' Xuuuuuu Xuu ';
			         notices: #( #( 2 1 2 'Variable or expression expected' )
				            #( 10 9 10 'Variable or expression expected' )
				            #( 14 13 14 'Variable or expression expected' ) )).
		        (self new
			         source: '1 goodby: my: ';
			         nodeAt: '10000000000000';
			         styled: 'n uuuuuuu Xuu ';
			         notices:
				         #( #( 11 10 11 'Variable or expression expected' )
				            #( 15 14 15 'Variable or expression expected' ) )).
		        (self new
			         source: '1 goodby: 2 my: ';
			         nodeAt: '1000000000200000';
			         styled: 'n uuuuuuu n uuu ';
			         notices:
				         #( #( 17 16 17 'Variable or expression expected' ) )).
		        (self new
			         source: ' goodby: 2 my: ';
			         nodeAt: ' 00000000200000';
			         styled: ' Xuuuuuu n uuu ';
			         notices: #( #( 2 1 2 'Variable or expression expected' )
				            #( 16 15 16 'Variable or expression expected' ) )).
		        (self new
			         source: ' goodby: my: 3';
			         nodeAt: ' 0000000000003';
			         styled: ' Xuuuuuu Xuu n';
			         notices: #( #( 2 1 2 'Variable or expression expected' )
				            #( 10 9 10 'Variable or expression expected' ) )).
		        (self new
			         source: '1 goodby: my: 3';
			         nodeAt: '100000000000003';
			         styled: 'n uuuuuuu Xuu n';
			         notices:
				         #( #( 11 10 11 'Variable or expression expected' ) )).
		        (self new
			         source: ' goodby: 2 my: 3';
			         nodeAt: ' 000000002000003';
			         styled: ' Xuuuuuu n uuu n';
			         notices: #( #( 2 1 2 'Variable or expression expected' ) )).
		        "Combinaisons"
		        (self new
			         source: ' + foo: - ';
			         nodeAt: ' 110000044';
			         styled: ' X Xsss X ';
			         notices: #( #( 2 1 2 'Variable or expression expected' )
				            #( 4 3 4 'Variable or expression expected' )
				            #( 9 8 9 'Variable or expression expected' )
				            #( 11 10 11 'Variable or expression expected' ) )).

		        "Bad assignments"
		        (self new
			         source: 'a := ';
			         nodeAt: '20000';
			         styled: 'u    ';
			         notices: #( #( 6 5 6 'Variable or expression expected' )
				            #( 1 1 1 'Undeclared variable' ) )).
		        (self new
			         source: ':= ';
			         nodeAt: '000';
			         styled: 'XX ';
			         notices: #( #( 4 3 4 'Variable or expression expected' )
				            #( 1 3 1 'variable expected in assigment' ) )).
		        (self new
			         source: ':= 2';
			         nodeAt: '0001';
			         styled: 'XX n';
			         notices: #( #( 1 4 1 'variable expected in assigment' ) )).
		        (self new
			         source: '1:=2';
			         nodeAt: '2334';
			         styled: 'nXXn';
			         formattedCode: '1. := 2';
			         notices: #( #( 1 1 2 'End of statement expected' )
				            #( 2 4 2 'variable expected in assigment' ) )).

		        "Bad cascades"
		        (self new
			         source: ';';
			         nodeAt: '0';
			         styled: 'X';
			         formattedCode: ' ; ';
			         notices: #( #( 1 0 1 'Variable or expression expected' )
				            #( 1 0 1 'Message expected' )
				            #( 2 1 2 'Cascade message expected' ) )).
		        (self new
			         source: '1;foo';
			         nodeAt: '43333';
			         styled: 'nXsss';
			         formattedCode: '1 ; foo';
			         notices: #( #( 1 1 2 'Message expected' ) )).
		        (self new
			         source: '1;';
			         nodeAt: '20';
			         styled: 'nX';
			         formattedCode: '1 ; ';
			         notices:
				         #( #( 1 1 2 'Message expected' )
				            #( 3 2 3 'Cascade message expected' ) )).
		        (self new
			         source: '1 sign;';
			         nodeAt: '2111110';
			         styled: 'n ssss;';
			         formattedCode: '1 sign; ';
			         notices: #( #( 8 7 8 'Cascade message expected' ) )).
		        (self new
			         source: '1 foo:;bar';
			         nodeAt: '5111114444';
			         styled: 'n ssssXsss';
			         formattedCode: '1 foo: ; bar';
			         notices: #( #( 7 6 7 'Variable or expression expected' ) )). "The cascade is correct here. It's a simple error of a missing argument"
		        (self new
			         source: '1 foo;2';
			         nodeAt: '4333326';
			         styled: 'n sss;X';
			         formattedCode: '1 foo; . 2';
			         notices: #( #( 7 6 7 'Cascade message expected' )
				            #( 1 6 7 'End of statement expected' ) )).
		        (self new
			         source: '(1 sign: 2);bar';
			         nodeAt: '676666666865555';
			         styled: '0n sssss n0Xsss';
			         formattedCode: '(1 sign: 2) ; bar';
			         notices: #( #( 1 11 12 'Message expected' ) )).
		        (self new
			         source: '(1 sign);bar';
			         nodeAt: '565555554444';
			         styled: '0n ssss0Xsss';
			         formattedCode: '1 sign ; bar';
			         notices: #( #( 1 8 9 'Message expected' ) )). "FIXME the parentheses are lost, and this changes the meaning"
		        "Longer cascade"
		        (self new
			         source: ';;';
			         nodeAt: '00';
			         styled: 'XX';
			         formattedCode: ' ; ; ';
			         notices: #( #( 1 0 1 'Variable or expression expected' )
				            #( 1 0 1 'Message expected' )
				            #( 2 1 2 'Cascade message expected' )
				            #( 3 2 3 'Cascade message expected' ) )).
		        (self new
			         source: '1 sign;;bar';
			         nodeAt: '51111144444';
			         styled: 'n ssss;Xsss';
			         formattedCode: '1 sign; ; bar';
			         notices: #( #( 8 7 8 'Cascade message expected' ) )).

		        "Bad returns"
		        (self new
			         source: '^ ';
			         nodeAt: '00';
			         styled: '^ ';
			         notices: #( #( 3 2 3 'Variable or expression expected' ) )).
		        (self new
			         source: '1+^2';
			         nodeAt: '3256';
			         styled: 'nsXn';
			         formattedCode: '1 + . ^ 2';
			         notices: #( #( 3 2 3 'Variable or expression expected' )
				            #( 1 2 3 'End of statement expected' ) )).
		        (self new
			         source: '1 foo: ^2';
			         nodeAt: '322222256';
			         styled: 'n ssss Xn';
			         formattedCode: '1 foo: . ^ 2';
			         notices: #( #( 8 7 8 'Variable or expression expected' )
				            #( 1 7 8 'End of statement expected' ) )).
		        (self new
			         source: '(^1)';
			         nodeAt: '1453';
			         styled: 'XXnX';
			         formattedCode: '( . ^ 1 )';
			         notices: #( #( 2 1 2 'Variable or expression expected' )
				            #( 1 1 2 ''')'' expected' )
				            #( 2 4 4 'Missing opener for closer: )' ) )). "^ can only appear a the begin of a statement, so a random ^ cause an unfinished statement error"
		        (self new
			         source: '^^1';
			         nodeAt: '245';
			         styled: '^Xn';
			         formattedCode: '^ . ^ 1';
			         notices: #( #( 2 1 2 'Variable or expression expected' )
				            #( 1 1 2 'End of statement expected' ) )). "Same spirit"
		        (self new
			         source: '[ ^ 1 ]';
			         nodeAt: '0022300';
			         styled: '0 ^ n 0';
			         isFaulty: false;
			         raise: BlockCannotReturn). "when the block is evaluated, the method is already gone."
		        (self new
			         source: '{ ^ 1 }';
			         nodeAt: '0011200';
			         styled: '0 ^ n 0';
			         isFaulty: false;
			         value: 1). "I did not expect this one to be legal"
		        (self new
			         source: '#(^1)';
			         nodeAt: '00120';
			         styled: '00#n0';
			         formattedCode: '#( #''^'' 1 )';
			         isFaulty: false;
			         value: #( #'^' 1 );
			         notices: #( #( 3 3 3 'Use a proper symbol literal' ) )). "Obviously..."
		        (self new
			         source: '#[ ^ 1 ]';
			         nodeAt: '00010200';
			         styled: 'XX X n X';
			         notices: #( #( 4 4 4 '8-bit integer expected' ) )).

		        "Unreachable code (warnings)"
		        "Unreachable analysis is very simple and targets statement that directly follows a return statement."
		        "Note that faulty code can still be executed without a RuntimeSyntaxError"
		        (self new
			         source: '^ 1. 2. ^ 3';
			         nodeAt: '11200300445';
			         styled: '^ n. n. ^ n';
			         isFaulty: false;
			         value: 1;
			         notices: #( #( 6 6 6 'Unreachable statement' ) )).
		        (self new
			         source: '[ ^ 1. 2. ^ 3 ]';
			         nodeAt: '002231141155600';
			         styled: '0 ^ n. n. ^ n 0';
			         isFaulty: false;
			         raise: BlockCannotReturn;
			         notices: #( #( 8 8 8 'Unreachable statement' ) )). "like [^1]"
		        (self new
			         source: '{ ^ 1. 2. ^ 3 }';
			         nodeAt: '001120030044500';
			         styled: '0 ^ n  n  ^ n 0';
			         isFaulty: false;
			         value: 1;
			         notices: #( #( 8 8 8 'Unreachable statement' ) )).
		        (self new
			         source: '[ ^ 1 ]. 2. ^ 3';
			         nodeAt: '113341100500667';
			         styled: '0 ^ n 0. n. ^ n';
			         isFaulty: false;
			         value: 3).
		        (self new
			         source: '{ ^ 1 }. 2. ^ 3';
			         nodeAt: '112231100400556';
			         styled: '0 ^ n 0. n. ^ n';
			         isFaulty: false;
			         value: 1). "This one could have been..."
		        (self new
			         source: 'true ifTrue: [ ^ 1 ] ifFalse: [ ^ 2 ]. ^ 3';
			         nodeAt: '2222111111111335563311111111117799A7700BBC';
			         styled: 'kkkk sssssss 0 ^ n 0 ssssssss 0 ^ n 0. ^ n';
			         isFaulty: false;
			         value: 1) "Not *syntactic* enough" }.

	"Setup default values"
	self new
		group: #badSimpleExpressions;
		isScripting: true;
		isFaulty: true;
		applyDefaultTo: list.
	^ list
]

{ #category : 'accessing' }
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
			         styled: 'X';
			         notices: #( #( 1 1 2 'Literal expected' ) )).
		        (self new
			         source: '$';
			         nodeAt: '0';
			         styled: 'X';
			         notices: #( #( 1 1 2 'Character expected' ) )).
		        (self new
			         source: ':';
			         nodeAt: '0';
			         styled: 'X';
			         notices: #( #( 1 1 1 'Unexpected token' ) )).
		        (self new
			         source: '';
			         nodeAt: '';
			         styled: '';
			         isFaulty: false). "emptyness is ok"

		        "Bad string literal"
		        "Note: the only cases are the missing closing quotes since everything inside is captured as is and there is no escape sequences or interpolation (yet?)"
		        (self new
			         source: '''hello';
			         nodeAt: '000000';
			         styled: 'XXXXXX';
			         notices: #( #( 1 6 7 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '''hello''''world';
			         nodeAt: '0000000000000';
			         styled: 'XXXXXXXXXXXXX';
			         notices:
				         #( #( 1 13 14 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '''';
			         nodeAt: '0';
			         styled: 'X';
			         notices: #( #( 1 1 2 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '''hello''''';
			         nodeAt: '00000000';
			         styled: 'XXXXXXXX';
			         notices: #( #( 1 8 9 'Unmatched '' in string literal.' ) )). "unclosed string that ends with an escaped quote"

		        "Bad symbol literal"
		        (self new
			         source: '#1';
			         nodeAt: '12';
			         styled: 'XX';
			         formattedCode: '#. 1';
			         notices: #( #( 1 1 2 'Literal expected' ) )). "Become a bad sequence"
		        (self new
			         source: '#1r0';
			         nodeAt: '1322';
			         styled: 'XXXu';
			         formattedCode: '#. 1 r0';
			         notices:
				         #( #( 1 1 2 'Literal expected' )
				            #( 2 2 3 'an integer greater than 1 as valid radix expected' ) )). "Two bad sequences"
		        (self new
			         source: '##';
			         nodeAt: '00';
			         styled: 'XX';
			         notices: #( #( 1 2 3 'Literal expected' ) )).
		        "Note: if quotes, same thing than strings"
		        (self new
			         source: '#''hello';
			         nodeAt: '0000000';
			         styled: 'XXXXXXX';
			         notices: #( #( 1 7 8 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '#''hello''''world';
			         nodeAt: '00000000000000';
			         styled: 'XXXXXXXXXXXXXX';
			         notices:
				         #( #( 1 14 15 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '#''';
			         nodeAt: '00';
			         styled: 'XX';
			         notices: #( #( 1 2 3 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '#''hello''''';
			         nodeAt: '000000000';
			         styled: 'XXXXXXXXX';
			         notices:
				         #( #( 1 9 10 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '###''hello';
			         nodeAt: '000000000';
			         styled: 'XXXXXXXXX';
			         notices:
				         #( #( 1 9 10 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '###''hello''''world';
			         nodeAt: '0000000000000000';
			         styled: 'XXXXXXXXXXXXXXXX';
			         notices:
				         #( #( 1 16 17 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '###''';
			         nodeAt: '0000';
			         styled: 'XXXX';
			         notices: #( #( 1 4 5 'Unmatched '' in string literal.' ) )).
		        (self new
			         source: '###''hello''''';
			         nodeAt: '00000000000';
			         styled: 'XXXXXXXXXXX';
			         notices:
				         #( #( 1 11 12 'Unmatched '' in string literal.' ) )).

		        "Bad numeric literal"
		        "Note: currently there is only 2 cases or bad numeric literal, both related to bad radix"
		        (self new
			         source: '2r';
			         nodeAt: '00';
			         styled: 'XX';
			         notices:
				         #( #( 1 2 3 'a digit between 0 and 1 expected' ) )).
		        (self new
			         source: '2rx';
			         nodeAt: '110';
			         styled: 'XXX';
			         formattedCode: '2r x';
			         notices:
				         #( #( 1 2 3 'a digit between 0 and 1 expected' ) )). "a bad number followed by a unary message send"
		        (self new
			         source: '2r3';
			         nodeAt: '112';
			         styled: 'XXX';
			         formattedCode: '2r. 3';
			         notices:
				         #( #( 1 2 3 'a digit between 0 and 1 expected' ) )). "a bad number followed by a number, causing a case of unfinished sequence"
		        (self new
			         source: '0r';
			         nodeAt: '10';
			         styled: 'XX';
			         formattedCode: '0 r';
			         notices:
				         #( #( 1 1 2 'an integer greater than 1 as valid radix expected' ) )).
		        (self new
			         source: '000rx';
			         nodeAt: '11100';
			         styled: 'XXXXu';
			         formattedCode: '000 rx';
			         notices:
				         #( #( 1 3 4 'an integer greater than 1 as valid radix expected' ) )).
		        (self new
			         source: '000r1';
			         nodeAt: '11100';
			         styled: 'XXXXu';
			         formattedCode: '000 r1';
			         notices:
				         #( #( 1 3 4 'an integer greater than 1 as valid radix expected' ) )).
		        (self new
			         source: '3r12345';
			         nodeAt: '2222333';
			         styled: 'nnnnXnn';
			         formattedCode: '3r12. 345';
			         notices: #( #( 1 4 5 'End of statement expected' ) )).

		        "These ones are correct, the number parser is very prermisive (except for radix, see above)"
		        (self new
			         source: '1.';
			         nodeAt: '0 ';
			         styled: 'n.';
			         formattedCode: '1';
			         isFaulty: false;
			         value: 1).
		        (self new
			         source: '1.1.1';
			         nodeAt: '11102';
			         styled: 'nnn.n';
			         formattedCode: '1.1. 1';
			         isFaulty: false;
			         value: 1).
		        (self new
			         source: '1a';
			         nodeAt: '10';
			         styled: 'ns';
			         formattedCode: '1 a';
			         isFaulty: false;
			         messageNotUnderstood: #a).
		        (self new
			         source: '1a1a1';
			         nodeAt: '10000';
			         styled: 'nuuuu';
			         formattedCode: '1 a1a1';
			         isFaulty: false;
			         messageNotUnderstood: #a1a1).
		        (self new
			         source: '1e';
			         nodeAt: '10';
			         styled: 'ns';
			         formattedCode: '1 e';
			         isFaulty: false;
			         messageNotUnderstood: #e).
		        (self new
			         source: '1e1e1';
			         nodeAt: '11100';
			         styled: 'nnnuu';
			         formattedCode: '1e1 e1';
			         isFaulty: false;
			         messageNotUnderstood: #e1).
		        (self new
			         source: '1s';
			         nodeAt: '00';
			         styled: 'nn';
			         isFaulty: false;
			         value: 1s0). "ScaledDecimal is a thing (!) that have literals (!!) inconsistent with '1e' (!!!)"
		        (self new
			         source: '1s1s1';
			         nodeAt: '11100';
			         styled: 'nnnuu';
			         formattedCode: '1s1 s1';
			         isFaulty: false;
			         messageNotUnderstood: #s1).
		        (self new
			         source: '10r89abcd';
			         nodeAt: '111110000';
			         styled: 'nnnnnuuuu';
			         formattedCode: '10r89 abcd';
			         isFaulty: false;
			         messageNotUnderstood: #abcd).
		        (self new
			         source: '12r89abcd';
			         nodeAt: '111111100';
			         styled: 'nnnnnnnuu';
			         formattedCode: '12r89ab cd';
			         isFaulty: false;
			         messageNotUnderstood: #cd).
		        (self new
			         source: '36r1halt';
			         nodeAt: '00000000';
			         styled: 'nnnnnnnn';
			         isFaulty: false;
			         value: 2486513). "ahah"

		        "Bad characters"
		        "Pharo is Unicode aware."
		        "$Δ isLetter >>> true" "$Δ asInteger >>> 16r0394" "Greek Capital Letter Delta"
		        "$ə isLetter >>> true" "$ə asInteger >>> 16r0259" "Latin Small Letter Schwa"
		        (self new
			         source: 'Δə';
			         nodeAt: '00';
			         styled: 'uu';
			         isParseFaulty: false;
			         isFaultyMinusUndeclared: false;
			         raise: UndeclaredVariableRead;
			         notices: #( #( 1 2 1 'Undeclared variable' ) )). "valid identifier"
		        (self new
			         source: '| Δə | Δə := 1. Δə + 1';
			         nodeAt: '0011000442222300665557';
			         styled: '| TT | tt    n. tt s n';
			         isFaulty: false;
			         value: 2).

		        "$± isSpecial >>> true" "$± asInteger >>> 16r00B1" "Plus Minus Sign" "Only a few unicode characters are isSpecial in Pharo"
		        (self new
			         source: '1 ± 1';
			         nodeAt: '10002';
			         styled: 'n u n';
			         isFaulty: false;
			         messageNotUnderstood: #±). "Valid binary operator, but not implemented"
		        "$→ isSpecial >>> false" "$→ asInteger hex >>> 16r2192" "Rightwards Arrow"
		        (self new
			         source: '→';
			         nodeAt: '0';
			         styled: 'X';
			         notices: #( #( 1 1 1 'Unknown character' ) )). "Unknown character. Not isSpecial"
		        "$٠ isDigit >>> true" "$٠ asInteger >>> 16r0660" "Arabic-indic Digit Zero"
		        (self new
			         source: '٠';
			         nodeAt: '0';
			         styled: 'X';
			         notices: #( #( 1 1 1 'Unknown character' ) )). "Unknown character. Not a valid number (basic ASCII only for numbers!)"
		        "Currently in Pharo, there is no 'isSeparator' character outside the ASCII range"
		        "Character nbsp isSeparator >>> false" "Even the standard nbsp"
		        (self new
			         source: Character nbsp asString;
			         nodeAt: '0';
			         styled: 'X';
			         notices: #( #( 1 1 1 'Unknown character' ) )). "Unknown character. Not isSeparator"
		        (self new
			         source: '$→';
			         nodeAt: '00';
			         styled: '$$';
			         isFaulty: false;
			         value: $→).
		        (self new
			         source: '''Δ→ə''';
			         nodeAt: '00000';
			         styled: '''''''''''';
			         isFaulty: false;
			         value: 'Δ→ə').
		        (self new
			         source: '"Δ→ə" ';
			         nodeAt: '22222 ';
			         styled: '""""" ';
			         isFaulty: false).
		        (self new
			         source: '#''Δ→ə''';
			         nodeAt: '000000';
			         styled: '######';
			         isFaulty: false;
			         value: #'Δ→ə').
		        (self new
			         source: '#Δə';
			         nodeAt: '000';
			         styled: '###';
			         formattedCode: '#''Δə''';
			         isFaulty: false;
			         value: #'Δə').
		        (self new
			         source: '#(Δ→ə)';
			         nodeAt: '001230';
			         styled: 'XX#X#X';
			         formattedCode: '#( Δ → ə )';
			         notices: #( #( 4 4 4 'Unknown character' ) )). "This one is faulty because → is a parse error"
		        (self new
			         source: '#( ( Δ → ə ) )';
			         nodeAt: '00011213141100';
			         styled: 'XX X # X # X X';
			         notices: #( #( 8 8 8 'Unknown character' ) )). "This one is faulty because → is a parse error"
		        (self new
			         source: '#( #( Δ → ə ) )';
			         nodeAt: '000111213141100';
			         styled: 'XX XX # X # X X';
			         notices: #( #( 9 9 9 'Unknown character' ) )). "This one is faulty because → is a parse error"
		        (self new
			         source: '#→';
			         nodeAt: '12';
			         styled: 'XX';
			         formattedCode: '#. →';
			         notices:
				         #( #( 1 1 2 'Literal expected' )
				            #( 2 2 2 'Unknown character' ) )) "Two independent errors" }.
	"Setup default values"
	self new
		group: #badTokens;
		isScripting: true;
		isFaulty: true;
		applyDefaultTo: list.
	^ list
]

{ #category : 'accessing' }
RBCodeSnippet class >> badVariableAndScopes [
	"Test various combinaisons of error and warnings on variables and scope nesting"

	<script: 'self styleWithError: self badVariableAndScopes'>
	| list |
	list := {
		        (self new
			         source: 'a := a. { [ :a }. a := a';
			         nodeAt: '311112004455555766A88889';
			         styled: 'u    u. X X    X. X    X';
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
			         styled: 'u    u. X    X   . X    X';
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
			         styled: 'u    u. X    0 :B 0. X    X';
			         formattedCode: 'a := a. [ :a | [ :a | ]. a := a';
			         raise: UndeclaredVariableRead;
			         notices:
				         #( #( 6 6 6 'Undeclared variable' )
				            #( 1 1 1 'Undeclared variable' )
				            #( 9 27 28 ''']'' expected' ) )).
		        (self new
			         source: 'a := a. { [ :a | }. a := a';
			         nodeAt: '31111200445555555766A88889';
			         styled: 'u    u. X X      X. X    X';
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
			         styled: 'u    u. X X      X    X X. X    X';
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
			         styled: 'u    u. 0 X X X XX X 0. X    X';
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
			         styled: 'u    u. 0 :B X 0. u    u';
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
			         styled: 'u    u. 0 :B | X b b XX b 0. X    X';
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
			         styled: '0 :B :B :B | | T t T | t s t s t 0';
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
			         styled:
				         'pppp A pp A pp A 0 :B :B :B | | T t T | t s t s t 0';
			         isScripting: false;
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
			         styled: 'X            | t t t | t s t s t';
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
			         styled:
				         'pppp A pp A pp A X            | t t t | t s t s t';
			         isScripting: false;
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
		isScripting: true;
		isFaulty: true;
		applyDefaultTo: list.
	^ list
]

{ #category : 'script' }
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

{ #category : 'updating' }
RBCodeSnippet class >> updateSnippets: snippets [
	"This script will update all definitions of all snippets.
	Beware of code loss and double check the generated definitions."

	"I did not manage to update all in a single transformation (with a preview!)
	So do them one by one with a progress bar :("

	<script: 'self updateSnippets: self allSnippets'>
	snippets
		do: [ :snippet |
			snippet definitionRefactoring ifNotNil: [ :refactoring |
				refactoring execute ] ]
		displayingProgress: 'Updating snippets...'
]

{ #category : 'accessing' }
RBCodeSnippet >> applyDefaultTo: aCollection [

	aCollection do: [ :each |
		each default: self.
		each isScripting ifNil: [ each isScripting: self isScripting ].
		each isFaulty ifNil: [ each isFaulty: self isFaulty ].
		each isParseFaulty ifNil: [ each isParseFaulty: self isParseFaulty ].
		each isParseFaulty ifNil: [ each isParseFaulty: each isFaulty ].
		each isFaultyMinusUndeclared ifNil: [ each isFaultyMinusUndeclared: each isFaulty ].
		each formattedCode ifNil: [ each formattedCode: each source ].
		self skippedTests ifNotNil: [ self skippedTests do: [ :name | each skip: name ] ]
	]
]

{ #category : 'accessing' }
RBCodeSnippet >> default: aRBCodeSnippet [

	default := aRBCodeSnippet
]

{ #category : 'updating' }
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

{ #category : 'updating' }
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
		  aStream
			  nextPutAll: '; styled: ';
			  print: self styledPattern.
		  self formattedCode = self source ifFalse: [
			  aStream
				  nextPutAll: '; formattedCode: ';
				  print: self formattedCode ].
		  self isScripting = default isScripting ifFalse: [
			  aStream
				  nextPutAll: '; isScripting: ';
				  print: self isScripting ].
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
			  | defaultSkips |
			  defaultSkips := default skippedTests ifNil: [ #(  ) ].
			  self skippedTests do: [ :each |
				  (defaultSkips includes: each) ifFalse: [
					  aStream
						  nextPutAll: '; skip: ';
						  print: each ] ] ].
		  aStream nextPut: $) ]
]

{ #category : 'updating' }
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

{ #category : 'accessing' }
RBCodeSnippet >> formattedCode [

	^ formattedCode
]

{ #category : 'accessing' }
RBCodeSnippet >> formattedCode: anObject [

	formattedCode := anObject
]

{ #category : 'accessing' }
RBCodeSnippet >> group [
	^ group
]

{ #category : 'accessing' }
RBCodeSnippet >> group: aString [
	group := aString
]

{ #category : 'testing' }
RBCodeSnippet >> hasAllNotices: someNotices [

	(self notices ifNil: [0] ifNotNil: [self notices size]) = someNotices size ifFalse: [ ^ false ].
	^ someNotices allSatisfy: [ :each | self hasNotice: each ]
]

{ #category : 'testing' }
RBCodeSnippet >> hasNotice: aNotice [

	self notices ifNil: [ ^ false ].
	^ self notices anySatisfy: [ :each |
		aNotice node start = each first
		& (aNotice node stop = each second)
		& (aNotice position = each third)
		& (aNotice messageText = each fourth) ]
]

{ #category : 'testing' }
RBCodeSnippet >> hasNotice: aString at: anInteger [

	self notices ifNil: [ ^ false ].

	^ self notices anySatisfy: [ :each |
		  each third = anInteger and: [ each fourth = aString ] ]
]

{ #category : 'testing' }
RBCodeSnippet >> hasUndeclared [

	self notices ifNil: [ ^ false ].
	^ self notices anySatisfy: [ :n | n fourth = 'Undeclared variable' ]
]

{ #category : 'accessing' }
RBCodeSnippet >> hasValue [

	^ hasValue ifNil: [ value isNotNil | isFaulty not ]
]

{ #category : 'accessing' }
RBCodeSnippet >> hasValue: anObject [

	hasValue := anObject
]

{ #category : 'asserting' }
RBCodeSnippet >> ifSkip: aTestName then: aBlock [

	skippedTests ifNil: [ ^ self ].
	(skippedTests includes: aTestName) ifFalse: [ ^ self ].
	^ aBlock value
]

{ #category : 'inspecting' }
RBCodeSnippet >> inspectionSource [
	<inspectorPresentationOrder: 35 title: 'Source'>

	^ self searchDefinition inspectionSource
]

{ #category : 'accessing' }
RBCodeSnippet >> isFaulty [

	^ isFaulty
]

{ #category : 'accessing' }
RBCodeSnippet >> isFaulty: anObject [

	isFaulty := anObject
]

{ #category : 'accessing' }
RBCodeSnippet >> isFaultyMinusUndeclared [

	^ isFaultyMinusUndeclared
]

{ #category : 'accessing' }
RBCodeSnippet >> isFaultyMinusUndeclared: anObject [

	isFaultyMinusUndeclared := anObject
]

{ #category : 'accessing' }
RBCodeSnippet >> isParseFaulty [

	^ isParseFaulty
]

{ #category : 'accessing' }
RBCodeSnippet >> isParseFaulty: anObject [

	isParseFaulty := anObject
]

{ #category : 'accessing' }
RBCodeSnippet >> isScripting [

	^ isScripting
]

{ #category : 'accessing' }
RBCodeSnippet >> isScripting: anObject [

	isScripting := anObject
]

{ #category : 'accessing' }
RBCodeSnippet >> messageNotUnderstood [

	^ messageNotUnderstood
]

{ #category : 'accessing' }
RBCodeSnippet >> messageNotUnderstood: anObject [

	messageNotUnderstood := anObject
]

{ #category : 'accessing' }
RBCodeSnippet >> nodeAt: anObject [
	"Synonym for `bestNodes:` used to be aligned with `source:`"

	nodePositions := anObject
]

{ #category : 'accessing' }
RBCodeSnippet >> nodePositions [

	^ nodePositions
]

{ #category : 'accessing' }
RBCodeSnippet >> nodePositions: anObject [

	nodePositions := anObject
]

{ #category : 'accessing' }
RBCodeSnippet >> notices [
	^ notices
]

{ #category : 'accessing' }
RBCodeSnippet >> notices: aCollection [
	notices := aCollection
]

{ #category : 'accessing' }
RBCodeSnippet >> numberOfCritiques [

	^ numberOfCritiques
]

{ #category : 'accessing' }
RBCodeSnippet >> numberOfCritiques: anObject [

	numberOfCritiques := anObject
]

{ #category : 'parsing' }
RBCodeSnippet >> parse [
	^ isScripting
		ifTrue: [ RBParser parseFaultyExpression: self source ]
		ifFalse: [ RBParser parseFaultyMethod: self source ]

]

{ #category : 'parsing' }
RBCodeSnippet >> parseOnError: aBlock [

	^ [ isScripting
			  ifTrue: [ RBParser parseExpression: self source ]
			  ifFalse: [ RBParser parseMethod: self source ] ]
		  on: CodeError
		  do: [ :e | aBlock value: e ]
]

{ #category : 'printing' }
RBCodeSnippet >> printOn: aStream [

	super printOn: aStream.
	source ifNotNil: [
		aStream
			nextPut: $(;
			nextPutAll: source;
			nextPut: $) ]
]

{ #category : 'accessing' }
RBCodeSnippet >> raise [

	^ raise
]

{ #category : 'accessing' }
RBCodeSnippet >> raise: anObject [

	raise := anObject
]

{ #category : 'updating' }
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

{ #category : 'accessing' }
RBCodeSnippet >> skip: aSymbol [

	skippedTests ifNil: [ skippedTests := OrderedCollection new ].
	skippedTests addIfNotPresent: aSymbol
]

{ #category : 'accessing' }
RBCodeSnippet >> skippedTests [

	^ skippedTests
]

{ #category : 'accessing' }
RBCodeSnippet >> skippedTests: anObject [

	skippedTests := anObject
]

{ #category : 'accessing' }
RBCodeSnippet >> source [

	^ source
]

{ #category : 'accessing' }
RBCodeSnippet >> source: anObject [

	source := anObject
]

{ #category : 'accessing' }
RBCodeSnippet >> styled: anObject [

	styledPattern := anObject
]

{ #category : 'accessing' }
RBCodeSnippet >> styledPattern [

	^ styledPattern
]

{ #category : 'accessing' }
RBCodeSnippet >> styledPattern: anObject [

	styledPattern := anObject
]

{ #category : 'inspecting' }
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

{ #category : 'updating' }
RBCodeSnippet >> updateDefinition [
	"Do the update of the definition of self in the method (class-side) that defines it"

	self definitionRefactoring ifNotNil: [ :transformation |
		transformation execute ]
]

{ #category : 'updating' }
RBCodeSnippet >> updateExpectations [
	"Update the value of expectations according what tools produce.
	Do not blindly trust it, as a tool cannot judge itself."

	| ast |
	ast := self parse.
	self formattedCode: ast formattedCode withSeparatorsCompacted.
	self isParseFaulty: ast isFaulty.
	self nodePositions: ast asPositionDebugString.
	ast := self doSemanticAnalysis.
	self isFaulty: ast isFaulty.
	self isFaultyMinusUndeclared: self isFaulty
		& (ast allErrorNotices allSatisfy: #isUndeclaredNotice) not.
	self styledPattern: self mockedStyledText.
	self notices: (ast allNotices
			 collect: [ :each |
				 {
					 each node start.
					 each node stop.
					 each position.
					 each messageText } ]
			 as: Array)
]

{ #category : 'accessing' }
RBCodeSnippet >> value [

	^ value
]

{ #category : 'accessing' }
RBCodeSnippet >> value: anObject [

	value := anObject.
	hasValue := true
]
