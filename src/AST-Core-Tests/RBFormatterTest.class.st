"
SUnit tests for BISimpleFormatter
"
Class {
	#name : #RBFormatterTest,
	#superclass : #RBParseTreeTest,
	#category : #'AST-Core-Tests-Formatter'
}

{ #category : #private }
RBFormatterTest >> formatClass: aClass [
	aClass selectors
		do: [ :each | self formatClass: aClass selector: each ]
]

{ #category : #private }
RBFormatterTest >> formatClass: aClass selector: aSymbol [
	| source tree1 tree2 |
	source := aClass sourceCodeAt: aSymbol.
	tree1 := self parserClass parseMethod: source.
	tree2 := self parserClass
		parseMethod: (self formatterClass new format: tree1)
		onError: [ :err :pos | self assert: false ].
	self assert: tree1 equals: tree2
]

{ #category : #private }
RBFormatterTest >> formatterClass [
  ^RBSimpleFormatter
]

{ #category : #tests }
RBFormatterTest >> testCoreSystem [
	#(#Object #Behavior #Boolean #True #False #Integer #SmallInteger #Collection #String)
		do: [ :each |
			| class |
			class := testingEnvironment classNamed: each.
			self
				formatClass: class;
				formatClass: class class ]
]

{ #category : #tests }
RBFormatterTest >> testFormatPragmaWithLastIsSymbolArgument [
	| inputSource tree outputSource |
	"formatter should not create invalid code by removing whitespace between #= and >"
	inputSource := 'foo  <selector: #= > ^ self'.
	tree := self parseMethod: inputSource.
	outputSource := self formatterClass new format: tree.
	self shouldnt: [self parseMethod: outputSource] raise: SyntaxErrorNotification.

	"already worked and still should for non-symbol arguments"
	inputSource := 'foo  <selector: 0 > ^ self'.
	tree := self parseMethod: inputSource.
	outputSource := self formatterClass new format: tree.
	self shouldnt: [self parseMethod: outputSource] raise: SyntaxErrorNotification.

	"already worked and should still work for pragmas without arguments"
	inputSource := 'foo  <selector> ^ self'.
	tree := self parseMethod: inputSource.
	outputSource := self formatterClass new format: tree.
	self shouldnt: [self parseMethod: outputSource] raise: SyntaxErrorNotification
]

{ #category : #tests }
RBFormatterTest >> testLiteralDynamicArray [
	| source tree1 |
	source := 'foo ^ { ''film'' . ''FILM''} '.
	tree1 := self parseMethod: source.
	self assert: ((self formatterClass new format: tree1) occurrencesOf: $.) equals: 1
]

{ #category : #tests }
RBFormatterTest >> testParseError [
	| inputSources inputSource expectedResult errorNode isok |
	"parse error nodes should have the faulty code.
	The list contains various (and possibly systematic) variations of faulty inputs (and some correct ones for good measure)."
	inputSources := {
		"Random alone special character (lone opening or closing charactee are managed in the following sections)"
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
		'#(1]2}3)' -> { #isok->true. #format->'#( 1 #'']'' 2 #''}'' 3 )' }. "`#(` can eat almost anything"
		'#[ 1 ) 2]'.
		'#[ 1 } 2]'.
		'#[ 1 a 2]'.
		'#[ 1 -1 2]'.
		'#[ 1 1.0 2]'.
		'#[ 1 256 2]'.
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
		'#[1]#[2]' -> ' #[ 1 ].<r>#[ 2 ]'.

		"Bad temporary variable declarations"
		'|'.
		'| a b'.
		'| 1' -> '|' "FIXME, where is 1?".
		"Note that the | character is also a binary operator, so a missing opening | become a binary call with a missing argument (see bellow)"
		'a | '.

		"Bad block parameters"
		'[:a b]' -> '[ :a : | b ]'. "FIXME"
		'[:a 1]' -> '[ :a : | 1 ]'. "FIXME"
		'[|]' -> '[ |'. "FIXME (where is the closing bracket)"

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

		"Bad assignments"
		'a := '.
		':= '.
		':= 2'.
		'1:=2' -> ' 1.<r>:= 2'.
		'1 foo := 2' -> ' 1 foo.<r>:= 2'.
		'(1:=2)' -> ' ( 1.<r> := 2)'.

		"Bad cascades"
		';' -> '<r><t>;<r><t>'.
		'1;foo' -> '1<r><t>;<r><t>foo'.
		'1;' -> '1<r><t>;<r><t>'.
		'1 foo;' -> '1<r><t>foo;<r><t>'.
		'1 foo:;bar' -> '1<r><t>foo: ;<r><t>bar'. "The cascade is correct here. It's a simple error of a missing argument"
		'1 foo;2' -> ' 1<r><t>foo;<r><t>.<r>2'.
		'(1 foo: 2);bar' -> '(1 foo: 2)<r><t>;<r><t>bar'.
		'(1 foo);bar' -> '1 foo<r><t>;<r><t>bar'. "BUT the parentheses are lost, but is changes the meaning"
		"Longer cascade"
		';;' -> '<r><t>;<r><t>;<r><t>'.
		'1 foo;;bar' -> '1<r><t>foo;<r><t>;<r><t>bar'.

		"Bad returns"
		'^ '.
		'1+^2' -> ' 1 + .<r>^ 2'.
		'1 foo: ^2' -> ' 1 foo: .<r>^ 2'.
		'(^1)' -> ' ( .<r> ^ 1)'. "FIXME WTF"
		'^^1' -> ' ^ .<r>^ 1' "FIXME, same spirit".
		'{ ^ 1 }' -> {#isok->true}. "I did not expect this one to be legal"
		'#(^1)' -> {#isok->true. #format->'#( #''^'' 1 )'}. "Obviously..."
		'#[ ^ 1]'.

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
		'1.' -> { #isok->true. #format->'1' }.
		'1.1.1' -> { #isok->true. #format->'1.1.<r>1' }.
		'1a' -> { #isok->true. #format->'1 a' }.
		'1a1a1' -> { #isok->true. #format->'1 a1a1' }.
		'1e' -> { #isok->true. #format->'1 e' }.
		'1e1e1' -> { #isok->true. #format->'1e1 e1' }.
		'1s' -> { #isok->true. #format->'1s' }. "ScaledDecimal is a thing (!) that have literals (!!) inconsistent with '1e' (!!!)"
		'1s1s1' -> { #isok->true. #format->'1s1 s1' }.
		'10r89abcd' -> { #isok->true. #format->'10r89 abcd' }.
		'12r89abcd' -> { #isok->true. #format->'12r89ab cd' }.
		'36r1halt' -> { #isok->true }. "ahah"
		}.
	inputSources do: [ :each |
		isok := false.
		each isAssociation ifTrue: [
			inputSource := expectedResult := each key expandMacros.
			each value isArray ifTrue: [
				| dict |
				dict := each value asDictionary.
				dict at: #isok ifPresent: [ :v | isok := v ].
				dict at: #format ifPresent: [ :v | expectedResult := v expandMacros ].
			] ifFalse: [
				expectedResult := each value expandMacros
			]
		] ifFalse: [
			inputSource := expectedResult := each expandMacros ].
	   errorNode := self parseFaultyExpression: inputSource.
		self assert: errorNode isFaulty equals: isok not.
		self assert: errorNode formattedCode equals: expectedResult ]
]

{ #category : #tests }
RBFormatterTest >> testPreserveLiteralArrayFormat [
	| inputSource literalArrayNode |
	"symbols within a literal array can omit the # character, if it is used that way,
	the formatter should not add a # character but just use the source form."
	inputSource := '#( #withnumbersign nonumbersign ''string'' )'.
   literalArrayNode := self parseExpression: inputSource.
	self assert: literalArrayNode source equals: literalArrayNode formattedCode
]

{ #category : #tests }
RBFormatterTest >> testPreserveLiteralNumberFormat [
	| inputSource numbersNode |
	"Literal numbers can be written in different ways, integer, integer with
	radix float, fractional constants. The formatter should not change the
	formatting."
	inputSource := '#( 1 4r33 16r0F 0.02 2e-2 -1 )'.
   numbersNode := self parseExpression: inputSource.
	self assert: numbersNode source equals: numbersNode formattedCode
]
