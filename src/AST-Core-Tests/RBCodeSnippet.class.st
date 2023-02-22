"
Small pieces of source code used used to test various AST based tools.

See `RBCodeSnippet allSnippets` for a various collection of instances.

* source <String> the source code of the snippet
* formatedCode <String> the expected reformated code
* isFaulty <Boolean> is the parser expected to produce a faulty AST 
* value <Object> the expected value when executed
* messageNotUnderstood <Symbol> the expected MNU error when executed
* skippedTests <Collection> list of test to not execute (that should be fixed)

Tests are executed by the parametrized matrix class `RBCodeSnipetTest`

Additional tools can add more information and expectation (please improve the list and the tools)
"
Class {
	#name : #RBCodeSnippet,
	#superclass : #Object,
	#instVars : [
		'source',
		'isFaulty',
		'value',
		'formattedCode',
		'skippedTests',
		'messageNotUnderstood'
	],
	#category : #'AST-Core-Tests-Snippets'
}

{ #category : #accessing }
RBCodeSnippet class >> allSnippets [

	^ self badExpressionSources2.
"	^ self badExpressions"
]

{ #category : #accessing }
RBCodeSnippet class >> badExpressionSources [

	"This list contains various (and possibly systematic) variations of faulty inputs (and some correct ones for good measure).
	Unless if specifically testing token handling (e.g. in the scanner) try to use the formater format `` as the source"
	^ {
		"Random alone special character (lone opening or closing characters are managed in others sections)"
		'#'.
		'$'.
		':'.

		"Bad compound"
		'( 1 + 2'.
		'#( 1 + 2'.
		'[ 1 + 2'.
		'#[ 1 2'.
		'{ 1 + 2'.
		' 1 + 2)'.
		' 1 + 2]'.
		' 1 + 2}'.

		'( '. "I dont know why a space here, but it is OK I guess"
		'#('.
		'['.
		'#['.
		'{'.

		"Compounds with an unexped thing inside"
		'(1]2)' -> '  ( 1].<r> 2)'.
		'(1}2)' -> '  ( 1}.<r> 2)'.
		'(1. 2)' -> '( 1.<r> 2)'.
		'[1)2]' -> '[<r>  1).<r>2 ]'.
		'[1}2]' -> '[<r>  1}.<r>2 ]'.
		'#(1]2}3)' -> { #isok->true. #format->'#( 1 #'']'' 2 #''}'' 3 )'. #value->#(1]2}3) }. "`#(` can eat almost anything"
		'#[ 1 ) 2 ]' -> {#skip->#(#testDump)}.
		'#[ 1 } 2 ]' -> {#skip->#(#testDump)} .
		'#[ 1 a 2 ]' -> {#skip->#(#testDump)}.
		'#[ 1 -1 2 ]' -> {#skip->#(#testDump)}.
		'#[ 1 1.0 2 ]' -> {#skip->#(#testDump)}.
		'#[ 1 256 2 ]' -> {#skip->#(#testDump)}.
		'{1)2}' -> '{<r><t>  1).<r><t>2 }'.
		'{1]2}' -> '{<r><t>  1].<r><t>2 }'.

		"...or without expected thing"
		"Note: all compounds `[]` `#()` `#[]` `{}` are legal empty, except one"
		'()' -> ''. "Errr... I understand but I do not like it"

		"Bad sequence. The first expression is considered unfinished."
		'1 2' -> ' 1.<r>2'.
		'1 foo 2' -> ' 1 foo.<r>2'.
		'(1)2' -> ' 1.<r>2'.
		'1(2)' -> ' 1.<r>2'.
		'(1)(2)' -> ' 1.<r>2'.
		'#hello#world' -> ' #hello.<r>#world'.
		'$h$w' -> ' $h.<r>$w'.
		'[1][2]' -> ' [ 1 ].<r>[ 2 ]'.
		'{1}{2}' -> ' { 1 }.<r>{ 2 }'.
		'#(1)#(2)' -> ' #( 1 ).<r>#( 2 )'.
		'#[1]#[2]' -> { #format->' #[ 1 ].<r>#[ 2 ]'. #skip->#(#testDump)}.

		"Bad temporary variable declarations"
		"Note: bad temporaries will be stored as error statements"
		'|'.
		'| a b'.
		'| 1' -> '|.<r>1'.
		"Note that the | character is also a binary operator, so a missing opening | become a binary call with a missing argument (see bellow)"
		'a | '.
		'a || '.
		'| | a' -> { #isok->true. #format->'a' }. "This one is legal, it is an empty list of temporaries, the | are dismissed"
		'|| a' -> { #isok->true. #format->'a' }. "Same, but are messing with the Scanner"
		' ||| a'. "this one is a binary operator ||| with a mising receiver"
		'| a | | b' -> '| a |<r> | b'. "A valid temporary followed by a binary operator with a missing receiver"
		'| a ||b' -> '| a.<r> || b'.

		"Unexpected parameters (or columns)"
		"Note that `:a` is not a token but a special `:` followed by an identifier, whereas `a:` is a single token."
		"Nevertheless, the parser will try to catch unexpected :a together"
		':a'.
		'::a' -> ' :.<r>:a'.
		':::a' -> ' :.<r> :.<r>:a'.
		'::' -> ' :.<r>:'.
		':a foo'.
		'a :foo' -> ' a.<r>:foo'.
		'a : foo' -> ' a.<r>:foo'.
		'a:' -> ' a: '. "keyword message with a missing receiver and argument"
		'a::'. "just a bad token"
		'a:foo' -> ' a: foo'. "keyword message with a missing receiver"
		'a::foo' -> ' a::.<r>foo'.
		':a:foo' -> ': a: foo'. "ok..."
		'|:a|' -> '|.<r>:a | '.
		'|:a' -> '|.<r>:a'.
		'|::a' -> '|.<r> :.<r>:a'.
		'|a:|' -> '|.<r> a:  | '.
		'|a:' -> '|.<r> a: '.

		"Bad block parameters"
		"A bad parameter cause a error object to be added as the last element of the block parameter.
		On formating, a double space can be seen."
		'[:a b]' -> { #format->'[ :a  | b ]'. #skip->#(#testExecute) }.
		'[:a 1]' -> { #format->'[ :a  | 1 ]'. #skip->#(#testExecute) }.
		'[:a :]' -> { #format->'[ :a : |  ]'. #skip->#(#testExecute) }.
		'[:a ::b]' -> { #format->'[ :a : |  ]'. #skip->#(#testExecute) }.
		'[:a :b]' -> { #isok->true. #format->'[ :a :b |  ]' }. "no pipe (and no body) is legal"
		'[: a : b]' -> { #isok->true. #format->'[ :a :b |  ]'}. "spaces are also legal"
		'[:a:b]' -> '[ :  |  a: b ]'. "`a:b` is not a legal identifier"
		'[  a:  ]'. "no parameters, a keyword message send witout receiver nor arguments"
		'[ | ]'.
		'[ | b ]'.
		'[ :a | | b ]'.
		'[ :a || b ]' -> '[ :a | | b ]'.
		'[:a| | |b]' -> { #isok->true. #format->'[ :a | b ]' }. "Explicit empty list of temporaries"
		'[:a| ||b]' -> { #isok->true. #format->'[ :a | b ]' }. "Same but mess with the Scanner"
		'[:a|| |b]' -> { #isok->true. #format->'[ :a | b ]' }. "Same"
		'[:a|||b]' -> '[ :a  |  ||| b ]'. "FIXME. the number of | does not even match"

		"Unclosed blocks"
		'[: | '.
		'[:' -> '[:  | '.
		'[:a :b | '.
		'[:a :b' -> '[:a :b  | '.
		'[:a b' -> '[:a  |  b'.
		'[:a | '.
		'[:a |  b'.
		'[ |'.
		'[ | 1'.
		'[ | a'.

		"Missing receiver or argument in message sends.
		Note: a unary message send without a receiver will be 'correctly' mistaken as a variable, so not a parsing error"
		"binary"
		' + '.
		'1 + '.
		' + 2'.
		"keywords"
		' hello: '.
		'1 hello: '.
		' hello: 2'.
		' goodby:  my: '.
		'1 goodby:  my: '.
		'1 goodby: 2 my: '.
		' goodby: 2 my: '.
		' goodby:  my: 3'.
		'1 goodby:  my: 3'.
		' goodby: 2 my: 3'.
		"Combinaisons"
		' +  foo:  - '.

		"Bad assignments"
		'a := '.
		':= '.
		':= 2'.
		'1:=2' -> ' 1.<r>:= 2'.

		"Bad cascades"
		';' -> '<r><t>;<r><t>'.
		'1;foo' -> '1<r><t>;<r><t>foo'.
		'1;' -> '1<r><t>;<r><t>'.
		'1 sign;' -> '1<r><t>sign;<r><t>'.
		'1 foo:;bar' -> '1<r><t>foo: ;<r><t>bar'. "The cascade is correct here. It's a simple error of a missing argument"
		'1 foo;2' -> ' 1<r><t>foo;<r><t>.<r>2'.
		'(1 sign: 2);bar' -> '(1 sign: 2)<r><t>;<r><t>bar'.
		'(1 sign);bar' -> '1 sign<r><t>;<r><t>bar'. "BUT the parentheses are lost, but is changes the meaning"
		"Longer cascade"
		';;' -> '<r><t>;<r><t>;<r><t>'.
		'1 sign;;bar' -> '1<r><t>sign;<r><t>;<r><t>bar'.

		"Bad returns"
		'^ '.
		'1+^2' -> ' 1 + .<r>^ 2'.
		'1 foo: ^2' -> ' 1 foo: .<r>^ 2'.
		'(^1)' -> ' ( .<r> ^ 1)'. "FIXME WTF"
		'^^1' -> ' ^ .<r>^ 1' "FIXME, same spirit".
		'{ ^ 1 }' -> {#isok->true. #value->1}. "I did not expect this one to be legal"
		'#(^1)' -> {#isok->true. #format->'#( #''^'' 1 )'. #value->#(^1)}. "Obviously..."
		'#[ ^ 1 ]' -> {#skip->#(#testDump)}.

		"Bad string literal"
		"Note: the only cases are the missing closing quotes since everything inside is captured as is and there is no escape sequences or interpolation (yet?)"
		'''hello'.
		'''hello''''world'.
		''''.
		'''hello'''''. "unclosed string that ends with an escaped quote"

		"Bad symbol literal"
		'#1' -> ' #.<r>1'. "Become a bad sequence"
		'##' -> '#'. "errr. ok?"
		"Note: if quotes, same thing than strings"
		'#''hello'.
		'#''hello''''world'.
		'#'''.
		'#''hello'''''.
		'###''hello'.
		'###''hello''''world'.
		'###'''.
		'###''hello'''''.

		"Bad numeric literal"
		"Note: currently there is only 2 cases or bad numeric literal, both related to bad radix"
		'2r'.
		'2rx' -> '2r x'. "a bad number followed by a unary message send"
		'2r3' -> ' 2r.<r>3'. "a bad number followed by a number, causing a case of unfinished sequence"
		'0r' -> '0 r'.
		'000rx' -> '000 rx'.
		'000r1' -> '000 r1'.
		'3r12345' -> ' 3r12.<r>345'.

		"These ones are correct, the number parser is very prermisive (except for radix, see above)"
		'1.' -> { #isok->true. #format->'1'. #value->1 }.
		'1.1.1' -> { #isok->true. #format->'1.1.<r>1'. #value->1 }.
		'1a' -> { #isok->true. #format->'1 a'. #mnu->#a }.
		'1a1a1' -> { #isok->true. #format->'1 a1a1'. #mnu->#a1a1 }.
		'1e' -> { #isok->true. #format->'1 e'. #mnu->#e }.
		'1e1e1' -> { #isok->true. #format->'1e1 e1'. #mnu->#e1 }.
		'1s' -> { #isok->true. #format->'1s'. #value->1s0 }. "ScaledDecimal is a thing (!) that have literals (!!) inconsistent with '1e' (!!!)"
		'1s1s1' -> { #isok->true. #format->'1s1 s1'. #mnu->#s1 }.
		'10r89abcd' -> { #isok->true. #format->'10r89 abcd'. #mnu->#abcd }.
		'12r89abcd' -> { #isok->true. #format->'12r89ab cd'. #mnu->#cd }.
		'36r1halt' -> { #isok->true. #value->2486513 }. "ahah"
		}
]

{ #category : #accessing }
RBCodeSnippet class >> badExpressionSources2 [

	"This list contains various (and possibly systematic) variations of faulty inputs (and some correct ones for good measure)."
	"Unless specifically testing token handling (e.g. in the scanner) try to use the formater format `formattedCode` as the source to simplify this file"
	| list |
	list := {
		"Random alone special character (lone opening or closing characters are managed in others sections)"
		self new source: '#'.
		self new source: '$'.
		self new source: ':'.

		"Bad compound"
		self new source: '( 1 + 2'.
		self new source: '#( 1 + 2'.
		self new source: '[ 1 + 2'.
		self new source: '#[ 1 2'.
		self new source: '{ 1 + 2'.
		self new source: ' 1 + 2)'.
		self new source: ' 1 + 2]'.
		self new source: ' 1 + 2}'.

		self new source: '( '. "I dont know why a space here, but it is OK I guess"
		self new source: '#('.
		self new source: '['.
		self new source: '#['.
		self new source: '{'.

		"Compounds with an unexped thing inside"
		self new source: '(1]2)'; formattedCode: '  ( 1].<r> 2)'.
		self new source: '(1}2)'; formattedCode: '  ( 1}.<r> 2)'.
		self new source: '(1. 2)'; formattedCode: '( 1.<r> 2)'.
		self new source: '[1)2]'; formattedCode: '[<r>  1).<r>2 ]'.
		self new source: '[1}2]'; formattedCode: '[<r>  1}.<r>2 ]'.
		self new source: '#(1]2}3)'; formattedCode:'#( 1 #'']'' 2 #''}'' 3 )'; isFaulty: false; value: #(1]2}3). "`#(` can eat almost anything"
		self new source: '#[ 1 ) 2 ]'; skip: #testDump.
		self new source: '#[ 1 } 2 ]'; skip: #testDump.
		self new source: '#[ 1 a 2 ]'; skip: #testDump.
		self new source: '#[ 1 -1 2 ]'; skip: #testDump.
		self new source: '#[ 1 1.0 2 ]'; skip: #testDump.
		self new source: '#[ 1 256 2 ]'; skip: #testDump.
		self new source: '{1)2}'; formattedCode: '{<r><t>  1).<r><t>2 }'.
		self new source: '{1]2}'; formattedCode: '{<r><t>  1].<r><t>2 }'.

		"...or without expected thing"
		"Note: all compounds `[]` `#()` `#[]` `{}` are legal empty, except one"
		self new source: '()'; formattedCode: ''. "Errr... I understand but I do not like it"

		"Bad sequence. The first expression is considered unfinished."
		self new source: '1 2'; formattedCode: ' 1.<r>2'.
		self new source: '1 foo 2'; formattedCode: ' 1 foo.<r>2'.
		self new source: '(1)2'; formattedCode: ' 1.<r>2'.
		self new source: '1(2)'; formattedCode: ' 1.<r>2'.
		self new source: '(1)(2)'; formattedCode: ' 1.<r>2'.
		self new source: '#hello#world'; formattedCode: ' #hello.<r>#world'.
		self new source: '$h$w'; formattedCode: ' $h.<r>$w'.
		self new source: '[1][2]'; formattedCode: ' [ 1 ].<r>[ 2 ]'.
		self new source: '{1}{2}'; formattedCode: ' { 1 }.<r>{ 2 }'.
		self new source: '#(1)#(2)'; formattedCode: ' #( 1 ).<r>#( 2 )'.
		self new source: '#[1]#[2]'; formattedCode: ' #[ 1 ].<r>#[ 2 ]'; skip: #testDump.

		"Bad temporary variable declarations"
		"Note: bad temporaries will be stored as error statements"
		self new source: '|'.
		self new source: '| a b'.
		self new source: '| 1'; formattedCode: '|.<r>1'.
		"Note that the | character is also a binary operator, so a missing opening | become a binary call with a missing argument (see bellow)"
		self new source: 'a | '.
		self new source: 'a || '.
		self new source: '| | a'; formattedCode: 'a'; isFaulty: false. "This one is legal, it is an empty list of temporaries, the | are dismissed"
		self new source: '|| a'; formattedCode: 'a'; isFaulty: false. "Same, but are messing with the Scanner"
		self new source: ' ||| a'; formattedCode: ' | a'. "this one is a emtyp temps and a binary operator | with a mising receiver"
		self new source: ' |||| a'; formattedCode: ' || a'. "this one is a emtyp temps and a binary operator || with a mising receiver"
		self new source: '| a | | b'; formattedCode: '| a |<r> | b'. "A valid temporary followed by a binary operator with a missing receiver"
		self new source: '| a ||b'; formattedCode: '| a |<r> | b'.

		"Unexpected parameters (or columns)"
		"Note that `:a` is not a token but a special `:` followed by an identifier, whereas `a:` is a single token."
		"Nevertheless, the parser will try to catch unexpected :a together"
		self new source: ':a'.
		self new source: '::a'; formattedCode: ' :.<r>:a'.
		self new source: ':::a'; formattedCode: ' :.<r> :.<r>:a'.
		self new source: '::'; formattedCode: ' :.<r>:'.
		self new source: ':a foo'.
		self new source: 'a :foo'; formattedCode: ' a.<r>:foo'.
		self new source: 'a : foo'; formattedCode: ' a.<r>:foo'.
		self new source: 'a:'; formattedCode: ' a: '. "keyword message with a missing receiver and argument"
		self new source: 'a::'. "just a bad token"
		self new source: 'a:foo'; formattedCode: ' a: foo'. "keyword message with a missing receiver"
		self new source: 'a::foo'; formattedCode: ' a::.<r>foo'.
		self new source: ':a:foo'; formattedCode: ': a: foo'. "ok..."
		self new source: '|:a|'; formattedCode: '|.<r>:a | '.
		self new source: '|:a'; formattedCode: '|.<r>:a'.
		self new source: '|::a'; formattedCode: '|.<r> :.<r>:a'.
		self new source: '|a:|'; formattedCode: '|.<r> a:  | '.
		self new source: '|a:'; formattedCode: '|.<r> a: '.

		"Bad block parameters"
		"A bad parameter cause a error object to be added as the last element of the block parameter.
		On formating, a double space can be seen."
		self new source: '[:a b]'; formattedCode: '[ :a  | b ]'; skip: #testExecute.
		self new source: '[:a 1]'; formattedCode: '[ :a  | 1 ]'; skip: #testExecute.
		self new source: '[:a :]'; formattedCode: '[ :a : |  ]'; skip: #testExecute.
		self new source: '[:a ::b]'; formattedCode: '[ :a : |  ]'; skip: #testExecute.
		self new source: '[:a :b]'; formattedCode: '[ :a :b |  ]'; isFaulty: false. "no pipe (and no body) is legal"
		self new source: '[: a : b]'; formattedCode: '[ :a :b |  ]'; isFaulty: false. "spaces are also legal"
		self new source: '[:a:b]'; formattedCode: '[ :  |  a: b ]'. "`a:b` is not a legal identifier"
		self new source: '[  a:  ]'. "no parameters, a keyword message send witout receiver nor arguments"
		self new source: '[ | ]'.
		self new source: '[ | b ]'.
		self new source: '[ :a | | b ]'.
		self new source: '[ :a || b ]'; formattedCode: '[ :a | | b ]'.
		self new source: '[:a| | |b]'; formattedCode: '[ :a | b ]'; isFaulty: false. "Explicit empty list of temporaries"
		self new source: '[:a| ||b]'; formattedCode: '[ :a | b ]'; isFaulty: false. "Same but mess with the Scanner"
		self new source: '[:a|| |b]'; formattedCode: '[ :a | b ]'; isFaulty: false. "Same"
		self new source: '[:a|||b]'; formattedCode: '[ :a | b ]'; isFaulty: false. "Same"
		self new source: '[:a||||b]'; formattedCode: '[ :a |  | b ]'. "Binary | with a missing receiver"

		"Unclosed blocks"
		self new source: '[: | '.
		self new source: '[:'; formattedCode: '[:  | '.
		self new source: '[:a :b | '.
		self new source: '[:a :b'; formattedCode: '[:a :b  | '.
		self new source: '[:a b'; formattedCode: '[:a  |  b'.
		self new source: '[:a | '.
		self new source: '[:a |  b'.
		self new source: '[ |'.
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
		self new source: ' goodby:  my: '.
		self new source: '1 goodby:  my: '.
		self new source: '1 goodby: 2 my: '.
		self new source: ' goodby: 2 my: '.
		self new source: ' goodby:  my: 3'.
		self new source: '1 goodby:  my: 3'.
		self new source: ' goodby: 2 my: 3'.
		"Combinaisons"
		self new source: ' +  foo:  - '.

		"Bad assignments"
		self new source: 'a := '.
		self new source: ':= '.
		self new source: ':= 2'.
		self new source: '1:=2'; formattedCode: ' 1.<r>:= 2'.

		"Bad cascades"
		self new source: ';'; formattedCode: '<r><t>;<r><t>'.
		self new source: '1;foo'; formattedCode: '1<r><t>;<r><t>foo'.
		self new source: '1;'; formattedCode: '1<r><t>;<r><t>'.
		self new source: '1 sign;'; formattedCode: '1<r><t>sign;<r><t>'.
		self new source: '1 foo:;bar'; formattedCode: '1<r><t>foo: ;<r><t>bar'. "The cascade is correct here. It's a simple error of a missing argument"
		self new source: '1 foo;2'; formattedCode: ' 1<r><t>foo;<r><t>.<r>2'.
		self new source: '(1 sign: 2);bar'; formattedCode: '(1 sign: 2)<r><t>;<r><t>bar'.
		self new source: '(1 sign);bar'; formattedCode: '1 sign<r><t>;<r><t>bar'. "BUT the parentheses are lost, but is changes the meaning"
		"Longer cascade"
		self new source: ';;'; formattedCode: '<r><t>;<r><t>;<r><t>'.
		self new source: '1 sign;;bar'; formattedCode: '1<r><t>sign;<r><t>;<r><t>bar'.

		"Bad returns"
		self new source: '^ '.
		self new source: '1+^2'; formattedCode: ' 1 + .<r>^ 2'.
		self new source: '1 foo: ^2'; formattedCode: ' 1 foo: .<r>^ 2'.
		self new source: '(^1)'; formattedCode: ' ( .<r> ^ 1)'. "FIXME WTF"
		self new source: '^^1'; formattedCode: ' ^ .<r>^ 1' "FIXME, same spirit".
		self new source: '{ ^ 1 }'; isFaulty: false; value: 1. "I did not expect this one to be legal"
		self new source: '#(^1)'; formattedCode: '#( #''^'' 1 )'; isFaulty: false; value: #(^1). "Obviously..."
		self new source: '#[ ^ 1 ]'; skip: #testDump.

		"Bad string literal"
		"Note: the only cases are the missing closing quotes since everything inside is captured as is and there is no escape sequences or interpolation (yet?)"
		self new source: '''hello'.
		self new source: '''hello''''world'.
		self new source: ''''.
		self new source: '''hello'''''. "unclosed string that ends with an escaped quote"

		"Bad symbol literal"
		self new source: '#1'; formattedCode: ' #.<r>1'. "Become a bad sequence"
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
		self new source: '2r3'; formattedCode: ' 2r.<r>3'. "a bad number followed by a number, causing a case of unfinished sequence"
		self new source: '0r'; formattedCode: '0 r'.
		self new source: '000rx'; formattedCode: '000 rx'.
		self new source: '000r1'; formattedCode: '000 r1'.
		self new source: '3r12345'; formattedCode: ' 3r12.<r>345'.

		"These ones are correct, the number parser is very prermisive (except for radix, see above)"
		self new source: '1.'; formattedCode: '1'; isFaulty: false; value:1.
		self new source: '1.1.1'; formattedCode: '1.1.<r>1'; isFaulty: false; value:1.
		self new source: '1a'; formattedCode: '1 a'; isFaulty: false; messageNotUnderstood: #a.
		self new source: '1a1a1'; formattedCode: '1 a1a1'; isFaulty: false; messageNotUnderstood: #a1a1.
		self new source: '1e'; formattedCode: '1 e'; isFaulty: false; messageNotUnderstood: #e.
		self new source: '1e1e1'; formattedCode: '1e1 e1'; isFaulty: false; messageNotUnderstood: #e1.
		self new source: '1s'; formattedCode: '1s'; isFaulty: false; value: 1s0. "ScaledDecimal is a thing (!) that have literals (!!) inconsistent with '1e' (!!!)"
		self new source: '1s1s1'; formattedCode: '1s1 s1'; isFaulty: false; messageNotUnderstood: #s1.
		self new source: '10r89abcd'; formattedCode: '10r89 abcd'; isFaulty: false; messageNotUnderstood: #abcd.
		self new source: '12r89abcd'; formattedCode: '12r89ab cd'; isFaulty: false; messageNotUnderstood: #cd.
		self new source: '36r1halt'; isFaulty: false; value: 2486513. "ahah"
		}.
	"Setup default values"
	list do: [ :each |
		each isFaulty ifNil: [ each isFaulty: true ].
		each formattedCode
			ifNil: [ each formattedCode: each source ]
			ifNotNil: [ each formattedCode: each formattedCode expandMacros ].
	].
	^ list
]

{ #category : #accessing }
RBCodeSnippet class >> badExpressions [

	"self badExpressions"

	^ self fromSources: self badExpressionSources
]

{ #category : #'instance creation' }
RBCodeSnippet class >> fromSources: sources [

	^ sources collect: [ :each | self new fromSource: each ]
]

{ #category : #script }
RBCodeSnippet class >> styleAll [
	"Display all snipets in a big styled text.
	Each sniped is styled independently."

	<script>
	| bigtext |
	bigtext := Text new.
	self badExpressions do: [ :each |
		bigtext ifNotEmpty: [ bigtext append: String cr ].
		bigtext append: each styledText ].
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

{ #category : #'instance creation' }
RBCodeSnippet >> fromSource: aSource [

	self isFaulty: true.
	aSource isAssociation
		ifTrue: [
			self source: aSource key expandMacros.
			self formattedCode: self source.
			aSource value isArray
				ifTrue: [
					| dict |
					dict := aSource value asDictionary.
					dict at: #isok ifPresent: [ :v | self isFaulty: v not ].
					dict at: #value ifPresent: [ :v | self value: v ].
					dict at: #skip ifPresent: [ :v | self skippedTests: v ].
					dict at: #mnu ifPresent: [ :v | self messageNotUnderstood: v ].
					dict at: #format ifPresent: [ :v | self formattedCode: v expandMacros ] ]
				ifFalse: [ self formattedCode: aSource value expandMacros ] ]
		ifFalse: [
			self source: aSource expandMacros.
			self formattedCode: self source ]
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
RBCodeSnippet >> messageNotUnderstood [

	^ messageNotUnderstood
]

{ #category : #accessing }
RBCodeSnippet >> messageNotUnderstood: anObject [

	messageNotUnderstood := anObject
]

{ #category : #parsing }
RBCodeSnippet >> parse [
	^ RBParser parseFaultyExpression: self source
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

{ #category : #accessing }
RBCodeSnippet >> value [

	^ value
]

{ #category : #accessing }
RBCodeSnippet >> value: anObject [

	value := anObject
]
