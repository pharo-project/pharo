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
		'value',
		'hasValue',
		'formattedCode',
		'skippedTests',
		'raise',
		'messageNotUnderstood',
		'numberOfCritiques'
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
		"Random alone special character (lone opening or closing characters are managed in others sections)"
		self new source: '#'.
		self new source: '$'.
		self new source: ':'.
		self new source: ''; isFaulty: false. "emptyness is ok"
		
		"Comments"
		"EFFormater is not the best here :("
		self new source: '"" '; isFaulty: false.
		self new source: '"nothing" '; isFaulty: false.
		self new source: '"com"1"ment"'; formattedCode: '1'; isFaulty: false; value: 1. "The comments are in the AST, the formatter just do not know to show them because we format only the node and not the whole method body"
		self new source: '"a" 1 "b". "c" 2 "d"'; formattedCode: '1. "a" "b" "c" 2 "d"'; isFaulty: false; value: 2. "a and b moved around. Formatter issue. FIXME?"
		self new source: '"unfinished'.
		self new source: '"also unfinished""'.
		self new source: '"'.
		self new source: '"""'.

		"Bad compound"
		self new source: '( 1 + 2'.
		self new source: '#( 1 + 2'.
		self new source: '[ 1 + 2'.
		self new source: '#[ 1 2'.
		self new source: '{ 1 + 2'.
		self new source: '1 + 2 )'.
		self new source: '1 + 2 ]'.
		self new source: '1 + 2 }'.

		self new source: '( '.
		self new source: '#( '.
		self new source: '[ '.
		self new source: '#[ '.
		self new source: '{ '.

		self new source: '{ [ ( '.
		self new source: ') ] }'.

		"Compounds with an unexped thing inside"
		self new source: '(1]2)'; formattedCode: '( 1 ]. 2 )'.
		self new source: '(1}2)'; formattedCode: '( 1 }. 2 )'.
		self new source: '(1. 2)'; formattedCode: '( 1. 2 )'.
		self new source: '[1)2]'; formattedCode: '[ 1 ). 2 ]'.
		self new source: '[1}2]'; formattedCode: '[ 1 }. 2 ]'.
		self new source: '#(1]2}3)'; formattedCode:'#( 1 #'']'' 2 #''}'' 3 )'; isFaulty: false; value: #(1]2}3). "`#(` can eat almost anything"
		self new source: '#( 0 1r2 4 )'; formattedCode: '#( 0 1 r2 4 )'. "Almost anything..."
		self new source: '#[ 1 ) 2 ]'.
		self new source: '#[ 1 } 2 ]'.
		self new source: '#[ 1 a 2 ]'.
		self new source: '#[ 1 -1 2 ]'.
		self new source: '#[ 1 1.0 2 ]'.
		self new source: '#[ 1 256 2 ]'.
		self new source: '{1)2}'; formattedCode: '{ 1 ). 2 }'.
		self new source: '{1]2}'; formattedCode: '{ 1 ]. 2 }'.

		"...or without expected thing"
		"Note: all compounds `[]` `#()` `#[]` `{}` are legal empty, except one"
		self new source: '()'.

		"Bad sequence. The first expression is considered unfinished."
		self new source: '1 2'; formattedCode: '1. 2'.
		self new source: '1 foo 2'; formattedCode: '1 foo. 2'.
		self new source: '(1)2'; formattedCode: '1. 2'.
		self new source: '1(2)'; formattedCode: '1. 2'.
		self new source: '(1)(2)'; formattedCode: '1. 2'.
		self new source: '#hello#world'; formattedCode: '#hello. #world'.
		self new source: '$h$w'; formattedCode: '$h. $w'.
		self new source: '[1][2]'; formattedCode: '[ 1 ]. [ 2 ]'.
		self new source: '{1}{2}'; formattedCode: '{ 1 }. { 2 }'.
		self new source: '#(1)#(2)'; formattedCode: '#( 1 ). #( 2 )'.
		self new source: '#[1]#[2]'; formattedCode: '#[ 1 ]. #[ 2 ]'.

		"Bad temporary variable declarations"
		"Note: bad temporaries will be stored as error statements"
		self new source: '| '.
		self new source: '| a b'.
		self new source: '| 1'; formattedCode: '| . 1'.
		"Note that the | character is also a binary operator, so a missing opening | become a binary call with a missing argument (see bellow)"
		self new source: 'a | '.
		self new source: 'a || '.
		self new source: '| | a'; formattedCode: 'a'; isFaulty: false. "This one is legal, it is an empty list of temporaries, the | are dismissed"
		self new source: '|| a'; formattedCode: 'a'; isFaulty: false. "Same, but are messing with the Scanner"
		self new source: ' ||| a'; formattedCode: ' | a'. "this one is a empty temps and a binary operator | with a mising receiver"
		self new source: ' |||| a'; formattedCode: ' || a'. "this one is a empty temps and a binary operator || with a mising receiver"
		self new source: '| a | | b'. "A valid temporary followed by a binary operator with a missing receiver"
		self new source: '| a ||b'; formattedCode: '| a | | b'. "same"

		"Unexpected parameters (or columns)"
		"Note that `:a` is not a token but a special `:` followed by an identifier, whereas `a:` is a single token."
		"Nevertheless, the parser will try to catch unexpected :a together"
		self new source: ':a'.
		self new source: '::a'.
		self new source: ':::a'.
		self new source: '::'.
		self new source: ':a foo'.
		self new source: 'a :foo'; formattedCode: 'a. :foo'.
		self new source: 'a : foo'; formattedCode: 'a. :foo'.
		self new source: 'a:'; formattedCode: ' a: '. "keyword message with a missing receiver and argument"
		self new source: 'a::'. "just a bad token"
		self new source: 'a:foo'; formattedCode: ' a: foo'. "keyword message with a missing receiver"
		self new source: 'a::foo'; formattedCode: 'a::. foo'.
		self new source: ':a:foo'; formattedCode: ': a: foo'.
		self new source: '|:a|'; formattedCode: '| . :a | '.
		self new source: '|:a'; formattedCode: '| . :a'.
		self new source: '|::a'; formattedCode: '| . ::a'.
		self new source: '|a:|'; formattedCode: '| . a: | '.
		self new source: '|a:'; formattedCode: '| . a: '.

		"Bad block parameters"
		"A bad parameter cause a error object to be added as the last element of the block parameter.
		On formating, a double space can be seen."
		self new source: '[:a b]'; formattedCode: '[ :a | b ]'; hasValue: true. "FIXME"
		self new source: '[:a 1]'; formattedCode: '[ :a | 1 ]'; hasValue: true; value: 1. "FIXME"
		self new source: '[:a :]'; formattedCode: '[ :a : | ]'; hasValue: true. "FIXME"
		self new source: '[:a ::b]'; formattedCode: '[ :a ::b | ]'; hasValue: true. "FIXME"
		self new source: '[:a :b]'; formattedCode: '[ :a :b | ]'; isFaulty: false. "no pipe (and no body) is legal"
		self new source: '[: a : b]'; formattedCode: '[ :a :b | ]'; isFaulty: false. "spaces are also legal"
		self new source: '[:a:b]'; formattedCode: '[ : | a: b ]'. "FIXME?"
		self new source: '[ a: ]'. "no parameters, a keyword message send witout receiver nor arguments"
		self new source: '[ | ]'.
		self new source: '[ | b ]'.
		self new source: '[ :a | | b ]'.
		self new source: '[ :a || b ]'; formattedCode: '[ :a | | b ]'.
		self new source: '[:a| | |b]'; formattedCode: '[ :a | b ]'; isFaulty: false. "Explicit empty list of temporaries"
		self new source: '[:a| ||b]'; formattedCode: '[ :a | b ]'; isFaulty: false. "Same but mess with the Scanner"
		self new source: '[:a|| |b]'; formattedCode: '[ :a | b ]'; isFaulty: false. "Same"
		self new source: '[:a|||b]'; formattedCode: '[ :a | b ]'; isFaulty: false. "Same"
		self new source: '[:a||||b]'; formattedCode: '[ :a | | b ]'. "same + binary | without receiver"

		"Unclosed blocks"
		self new source: '[ : | '.
		self new source: '[:'; formattedCode: '[ : | '.
		self new source: '[ :a :b | '.
		self new source: '[ :a :b'; formattedCode: '[ :a :b | '.
		self new source: '[ :a b'; formattedCode: '[ :a | b'.
		self new source: '[ :a | '.
		self new source: '[ :a | b'.
		self new source: '[ | '.
		self new source: '[ | 1'.
		self new source: '[ | a'.

		"Missing receiver or argument in message sends.
		Note: a unary message send without a receiver will be 'correctly' mistaken as a variable, so not a parsing error"
		"binary"
		self new source: ' + '.
		self new source: '1 + '.
		self new source: ' + 2'.
		"keywords"
		self new source: ' hello: '.
		self new source: '1 hello: '.
		self new source: ' hello: 2'.
		self new source: ' goodby: my: '.
		self new source: '1 goodby: my: '.
		self new source: '1 goodby: 2 my: '.
		self new source: ' goodby: 2 my: '.
		self new source: ' goodby: my: 3'.
		self new source: '1 goodby: my: 3'.
		self new source: ' goodby: 2 my: 3'.
		"Combinaisons"
		self new source: ' + foo: - '.

		"Bad assignments"
		self new source: 'a := '.
		self new source: ':= '.
		self new source: ':= 2'.
		self new source: '1:=2'; formattedCode: '1. := 2'.

		"Bad cascades"
		self new source: ';'; formattedCode: ' ; '.
		self new source: '1;foo'; formattedCode: '1 ; foo'.
		self new source: '1;'; formattedCode: '1 ; '.
		self new source: '1 sign;'; formattedCode: '1 sign; '.
		self new source: '1 foo:;bar'; formattedCode: '1 foo: ; bar'. "The cascade is correct here. It's a simple error of a missing argument"
		self new source: '1 foo;2'; formattedCode: '1 foo; . 2'.
		self new source: '(1 sign: 2);bar'; formattedCode: '(1 sign: 2) ; bar'.
		self new source: '(1 sign);bar'; formattedCode: '1 sign ; bar'. "FIXME the parentheses are lost, and this changes the meaning"
		"Longer cascade"
		self new source: ';;'; formattedCode: ' ; ; '.
		self new source: '1 sign;;bar'; formattedCode: '1 sign; ; bar'.

		"Bad returns"
		self new source: '^ '.
		self new source: '1+^2'; formattedCode: '1 + . ^ 2'.
		self new source: '1 foo: ^2'; formattedCode: '1 foo: . ^ 2'.
		self new source: '(^1)'; formattedCode: '( . ^ 1 )'. "^ can only appear a the begin of a statement, so a random ^ cause an unfinished statement error"
		self new source: '^^1'; formattedCode: '^ . ^ 1' "Same spirit".
		self new source: '[ ^ 1 ]'; isFaulty: false; raise: BlockCannotReturn. "when the block is evaluated, the method is already gone."
		self new source: '{ ^ 1 }'; isFaulty: false; value: 1. "I did not expect this one to be legal"
		self new source: '#(^1)'; formattedCode: '#( #''^'' 1 )'; isFaulty: false; value: #(^1). "Obviously..."
		self new source: '#[ ^ 1 ]'.

		"Unreachable code"
		"Unreachable analysis is very simple and targets statement that dirrecty follows a return statement."
		"Note that faulty code can still be executed without a RuntimeSyntaxError"
		self new source: '^ 1. 2. ^ 3'; value: 1.
		self new source: '[ ^ 1. 2. ^ 3 ]'; raise: BlockCannotReturn. "like [^1]"
		self new source: '{ ^ 1. 2. ^ 3 }'; value: 1.
		self new source: '[ ^ 1 ]. 2. ^ 3'; isFaulty: false; value: 3.
		self new source: '{ ^ 1 }. 2. ^ 3'; isFaulty: false; value: 1. "This one could have been..."
		self new source: 'true ifTrue: [ ^ 1 ] ifFalse: [ ^ 2 ]. ^ 3'; isFaulty: false; value: 1. "Not *syntactic* enough"

		"Bad string literal"
		"Note: the only cases are the missing closing quotes since everything inside is captured as is and there is no escape sequences or interpolation (yet?)"
		self new source: '''hello'.
		self new source: '''hello''''world'.
		self new source: ''''.
		self new source: '''hello'''''. "unclosed string that ends with an escaped quote"

		"Bad symbol literal"
		self new source: '#1'; formattedCode: '#. 1'. "Become a bad sequence"
		self new source: '#1r0'; formattedCode: '#. 1 r0'. "Two bad sequences"
		self new source: '##'; formattedCode: '#'. "errr. ok?"
		"Note: if quotes, same thing than strings"
		self new source: '#''hello'.
		self new source: '#''hello''''world'.
		self new source: '#'''.
		self new source: '#''hello'''''.
		self new source: '###''hello'.
		self new source: '###''hello''''world'.
		self new source: '###'''.
		self new source: '###''hello'''''.

		"Bad numeric literal"
		"Note: currently there is only 2 cases or bad numeric literal, both related to bad radix"
		self new source: '2r'.
		self new source: '2rx'; formattedCode: '2r x'. "a bad number followed by a unary message send"
		self new source: '2r3'; formattedCode: '2r. 3'. "a bad number followed by a number, causing a case of unfinished sequence"
		self new source: '0r'; formattedCode: '0 r'.
		self new source: '000rx'; formattedCode: '000 rx'.
		self new source: '000r1'; formattedCode: '000 r1'.
		self new source: '3r12345'; formattedCode: '3r12. 345'.

		"These ones are correct, the number parser is very prermisive (except for radix, see above)"
		self new source: '1.'; formattedCode: '1'; isFaulty: false; value:1.
		self new source: '1.1.1'; formattedCode: '1.1. 1'; isFaulty: false; value:1.
		self new source: '1a'; formattedCode: '1 a'; isFaulty: false; messageNotUnderstood: #a.
		self new source: '1a1a1'; formattedCode: '1 a1a1'; isFaulty: false; messageNotUnderstood: #a1a1.
		self new source: '1e'; formattedCode: '1 e'; isFaulty: false; messageNotUnderstood: #e.
		self new source: '1e1e1'; formattedCode: '1e1 e1'; isFaulty: false; messageNotUnderstood: #e1.
		self new source: '1s'; isFaulty: false; value: 1s0. "ScaledDecimal is a thing (!) that have literals (!!) inconsistent with '1e' (!!!)"
		self new source: '1s1s1'; formattedCode: '1s1 s1'; isFaulty: false; messageNotUnderstood: #s1.
		self new source: '10r89abcd'; formattedCode: '10r89 abcd'; isFaulty: false; messageNotUnderstood: #abcd.
		self new source: '12r89abcd'; formattedCode: '12r89ab cd'; isFaulty: false; messageNotUnderstood: #cd.
		self new source: '36r1halt'; isFaulty: false; value: 2486513. "ahah"

		"Bad characters"
		"Pharo is Unicode aware."
		"$Δ isLetter >>> true" "$Δ asInteger >>> 16r0394" "Greek Capital Letter Delta"
		"$ə isLetter >>> true" "$ə asInteger >>> 16r0259" "Latin Small Letter Schwa"
		self new source: 'Δə'; isFaulty: false. "valid identifier"
		self new source: '| Δə | Δə := 1. Δə + 1'; isFaulty: false; value: 2.

		"$± isSpecial >>> true" "$± asInteger >>> 16r00B1" "Plus Minus Sign" "Only a few unicode characters are isSpecial in Pharo"
		self new source: '1 ± 1'; isFaulty: false; messageNotUnderstood: #'±'. "Valid binary operator, but not implemented"
		"$→ isSpecial >>> false" "$→ asInteger hex >>> 16r2192" "Rightwards Arrow"
		self new source: '→'. "Unknown character. Not isSpecial"
		"$٠ isDigit >>> true" "$٠ asInteger >>> 16r0660" "Arabic-indic Digit Zero"
		self new source: '٠'. "Unknown character. Not a valid number (basic ASCII only for numbers!)"
		"Currently in Pharo, there is no 'isSeparator' character outside the ASCII range"
		"Character nbsp isSeparator >>> false" "Even the standard nbsp"
		self new source: Character nbsp asString. "Unknown character. Not isSeparator"

		self new source: '$→'; isFaulty: false; value: $→.
		self new source: '''Δ→ə'''; isFaulty: false; value: 'Δ→ə'.
		self new source: '"Δ→ə" '; isFaulty: false.
		self new source: '#''Δ→ə'''; isFaulty: false; value: #'Δ→ə'.
		self new source: '#Δə'; formattedCode: '#''Δə''' ; isFaulty: false; value: #'Δə'.
		self new source: '#(Δ→ə)'; formattedCode: '#( Δ → ə )'. "This one is faulty because → is a parse error"
		self new source: '#→'; formattedCode: '#. →'. "Two independent errors"
		}.
	"Setup default values"
	list do: [ :each |
		each isMethod: false.
		each isFaulty ifNil: [ each isFaulty: true ].
		each formattedCode ifNil: [ each formattedCode: each source ].
	].
	^ list
]

{ #category : #accessing }
RBCodeSnippet class >> badMethods [

	| list |
	list := {
		"Emptyness"
		self new source: ' '.

		"Wrong token for pattern"
		"An empty pattern will be set, and the first statement will be a error node."
		self new source: '5'; formattedCode: ' . 5'.
		self new source: '''hello'''; formattedCode: ' . ''hello'''.
		self new source: '#hello'; formattedCode: ' . #hello'.
		self new source: ':'; formattedCode: ' . :'.
		self new source: '#(foo bar)'; formattedCode: ' . #( foo bar )'.

		"Random bad token"
		"Tought: is complainng about the bad token really better than complaining about the bad pattern?"
		self new source: ' $'.
		self new source: ' ''hello'.
		self new source: ' 2r3'; formattedCode: ' 2r. 3'.

		"Bad aruments"
		"The missing argument will be an error, the remainer starts the body"
		self new source: '+ '; hasValue: true.
		self new source: '+ 1'; hasValue: true.
		self new source: '+ foo: '.
		self new source: 'foo: '; hasValue: true.
		self new source: 'foo: 1'; hasValue: true.
		self new source: 'foo: + '.
		self new source: 'foo: bar: '; hasValue: true.
		self new source: 'foo:bar:'; formattedCode: ' . foo:bar:'. "`foo:bar:` is a single token, and is unexpected"

		"Bad pragma message"
		self new source: 'foo < '; hasValue: true.
		self new source: 'foo <> '.
		self new source: 'foo < 4'; hasValue: true.
		self new source: 'foo < bar '; hasValue: true.
		self new source: 'foo < bar: '; hasValue: true.
		self new source: 'foo < bar: 1 1 > '.
		self new source: 'foo < bar ; baz > '; formattedCode: 'foo < bar ; baz. > '.

		"Bad pragma value"
		self new source: 'foo <bar: > '; hasValue: true.
		self new source: 'foo <bar:(1)>'; formattedCode: 'foo < bar: 1 > '. "FIXME. dont eat parentheses"
		self new source: 'foo < bar: baz > '.
		self new source: 'foo < bar: 1 + 1 > '.
		self new source: 'foo < bar: [ 1 ] > '.
		self new source: 'foo < bar: { 1 } > '.
		self new source: 'foo <bar: #[ -1 ]> '; hasValue: true. "Literal bytes arrays are acceptable, but this one is faulty"
		self new source: 'foo < + 1> '; isFaulty: false. "Binary message is legal pragma"
		self new source: 'foo < + > '; hasValue: true.
		}.
	"Setup default values"
	list do: [ :each |
		each isMethod: true.
		each isFaulty ifNil: [ each isFaulty: true ].
		each formattedCode ifNil: [ each formattedCode: each source ].
	].
	^ list
]

{ #category : #accessing }
RBCodeSnippet class >> badSemantic [

	"List of varions semantic and backend errors."
	| list |
	list := {
		"Undefined variable"
		"For some reason, this is just a warning in non-faulty mode, not an error."
		self new source: 'a := 10. ^ a'; isFaulty: false; value: nil. "Weird, I expected to be 10"
		self new source: '^ a'; isFaulty: false; value: nil.

		"Uninitialized variable"
		self new source: '| a | ^ a'; value: nil.
		self new source: '| a | [ a := 10 ]. ^ a'; value: nil.
		self new source: '| a | [ ^ a ]. a := 10'; value: 10.

		"Duplicated variable definition (same scope)"
		self new source: 'foo: a bar: a ^ a'; value: 1; isMethod: true.
		self new source: '| a a | a := 10. ^ a'; value: 10.
		self new source: '[ | a a | a := 10. a ]'; value: 10.
		self new source: '[ :a :a | a ]'; value: 1.

		"Shadowed variables"
		self new source: 'foo: a ^ [ | a | a := 10. a ] value + a'; isMethod: true; value: 11.
		self new source: 'foo | a | a := 1. ^ [ | a | a := 10. a ] value + a'; isMethod: true; value: 11.
		self new source: 'foo ^ [ | a | a := 1. [ | a | a := 10. a ] value + a ] value'; isMethod: true; value: 11.
		self new source: 'foo ^ [ :a | [ | a | a := 10. a ] value + a ] value: 1'; isMethod: true; value: 11.
		self new source: 'foo: a ^ [ :a | a ] value: 10 + a'; isMethod: true; value: 11.
		self new source: 'foo | a | a := 1. ^ [ :a | a ] value: 10 + a'; isMethod: true; value: 11.
		self new source: 'foo ^ [ | a | a := 1. [ :a | a ] value: 10 + a ] value'; isMethod: true; value: 11.
		self new source: 'foo ^ [ :a | [ :a | a ] value: 10 + a ] value: 1'; isMethod: true; value: 11. "phonyArgs"

		"Write on readonly or reserved"
		self new source: 'foo: a a := 10. ^ a'; isMethod: true; isFaulty: true.
		self new source: '[ :a | a := 10. a ]'; isFaulty: true.
		"The following assignment are explicitely no-op, to minimize the impact if, for some broken reasons, they are really executed"
		self new source: 'nil := nil'; isFaulty: true; isParseFaulty: true; formattedCode: 'nil. := nil'. "it is a literal, not an identifier"
		self new source: 'true := true'; isFaulty: true; isParseFaulty: true; formattedCode: 'true. := true'. "it is a literal, not an identifier"
		self new source: 'false := false'; isFaulty: true; isParseFaulty: true; formattedCode: 'false. := false'. "it is a literal, not an identifier"
		self new source: 'self := self'; isFaulty: true; numberOfCritiques: 1.
		self new source: 'super := super'; isFaulty: true; numberOfCritiques: 1.
		self new source: 'thisContext := thisContext'; isFaulty: true; numberOfCritiques: 1.
		self new source: 'Object := Object'; isFaulty: true; numberOfCritiques: 1.

		"Shadowed reserved or global"
		self new source: '| self | self := 1. ^ self'; value: 1.
		self new source: '| super | super := 1. ^ super'; value: 1.
		self new source: '| thisContext | thisContext := 1. ^ thisContext'; value: 1.
		self new source: '| Object | Object := 1. ^ Object'; value: 1.
		self new source: 'foo: self ^ self + 1'; isMethod: true; value: 2.
		self new source: 'foo: super ^ super + 1'; isMethod: true; value: 2.
		self new source: 'foo: thisContext ^ thisContext + 1'; isMethod: true; value: 2.
		self new source: 'foo: Object ^ Object + 1'; isMethod: true; value: 2.
		self new source: '[ :self | self + 1 ]'; value: 2.
		self new source: '[ :super | super + 1 ]'; value: 2.
		self new source: '[ :thisContext | thisContext + 1 ]'; value: 2.
		self new source: '[ :Object | Object + 1 ]'; value: 2.

		"Backend errors"
		"FIXME: syntax error are thrown by the compiler even in *faulty* mode.
		 FIXME: semantic analysis does not catch the error (backend issue)"
		self new source: 'foo ^ [ :a1 :a2 :a3 :a4 :a5 :a6 :a7 :a8 :a9 :a10 :a11 :a12 :a13 :a14 :a15 :a16 | a1 ]'; isMethod: true; isFaulty: true. "Too many arguments"
		self new source: 'a1: a1 a2: a2 a3: a3 a4: a4 a5: a5 a6: a6 a7: a7 a8: a8 a9: a9 a10: a10 a11: a11 a12: a12 a13: a13 a14: a14 a15: a15 a16: a16 ^ a1'; isMethod: true; isFaulty: true. "Too many arguments"
		}.
	"Setup default values"
	list do: [ :each |
		each isMethod ifNil: [ each isMethod: false ].
		each isFaulty ifNil: [ each isFaulty: false ].
		each isParseFaulty ifNil: [ each isParseFaulty: false ].
		each formattedCode ifNil: [ each formattedCode: each source ]
	].
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
					with: (exception errorMessage asText addAttribute:
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
			node propertyAt: #semanticWarning ifPresent: [ :message |
				text := each textWithNode: node message: message.
				bigtext append: String cr.
				bigtext append: String tab.
				bigtext append: text.
				bigtext append: '   '.
				bigtext append: (node class name asText makeAllColor: Color gray).
				bigtext append: (' (semanticWarning)' asText makeAllColor: Color gray).
			].
			node isError ifTrue: [
				text := each textWithNode: node message: node errorMessage.
				bigtext append: String cr.
				bigtext append: String tab.
				bigtext append: text.
				bigtext append: '   '.
				bigtext append: (node class name asText makeAllColor: Color gray).
				bigtext append: (' (error)' asText makeAllColor: Color gray).
			].
		] ].
	bigtext inspect
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
		  on: SyntaxErrorNotification
		  do: [ :e | aBlock value: e ]
]

{ #category : #printing }
RBCodeSnippet >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: source;
		nextPut: $)
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
RBCodeSnippet >> textWithNode: aNode message: aString [

	| text stop |
	text := self styledText.
	stop := aNode stop min: text size.
	"Highlight the error node background"
	text
		addAttribute: (TextBackgroundColor color: Color cyan)
		from: aNode start
		to: stop.
	"Insert the message after the stop so that:
	 1. we can see where are zero-width errors
	 2. we have the same output than the compiler error (above)"
	text
		replaceFrom: stop + 1
		to: stop
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
