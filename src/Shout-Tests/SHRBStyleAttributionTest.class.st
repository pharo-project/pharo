Class {
	#name : 'SHRBStyleAttributionTest',
	#superclass : 'TestCase',
	#instVars : [
		'styler',
		'oldSetting'
	],
	#classVars : [
		'Test'
	],
	#pools : [
		'SHRBMockSharedPoolStyler'
	],
	#category : 'Shout-Tests-Styling',
	#package : 'Shout-Tests',
	#tag : 'Styling'
}

{ #category : 'asserting' }
SHRBStyleAttributionTest >> assertStyleOf: aText at: index shouldBe: aStyleSymbol [

	self
		assertStyleOf: aText
		between: index
		and: index
		shouldBe: aStyleSymbol
]

{ #category : 'asserting' }
SHRBStyleAttributionTest >> assertStyleOf: aText between: min and: max shouldBe: aStyleSymbol [

	min to: max do: [ :index | self assert: (aText attributesAt: index) anyOne name equals: aStyleSymbol ]
]

{ #category : 'initialization' }
SHRBStyleAttributionTest >> initialize [
		<ignoreUnusedClassVariables: #( Test )>
		super initialize.
]

{ #category : 'running' }
SHRBStyleAttributionTest >> setUp [

	super setUp.
	styler := SHRBMockTextStyler new.
	styler declared: #( #/ #setUp #style: #testKeywordStyle #print ).
	oldSetting := SHRBMockTextStyler instVarNamed: #formatIncompleteIdentifiers
]

{ #category : 'accessing' }
SHRBStyleAttributionTest >> style: aText [

	| ast |
	ast := self class compiler parse: aText asString.
	styler style: aText ast: ast.

	^ ast
]

{ #category : 'running' }
SHRBStyleAttributionTest >> tearDown [

	styler class formatIncompleteIdentifiers: oldSetting.
	super tearDown
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testArgumentStyle [

	| aText |
	aText := 'm: arg ^ arg' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 10
		and: 12
		shouldBe: #argument
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testAssignmentStyle [

	| aText |
	aText := 'm ^ styler := 2' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 5
		and: 10
		shouldBe: #instVar.
	self assertStyleOf: aText at: 15 shouldBe: #number
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testAttributeInBlockHasRightStyle [

	| aText |
	aText := 'm: arg self do: [ :each | arg ]' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 27
		and: 29
		shouldBe: #argument
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testBinaryStyle [

	| aText |
	aText := 'm self / 3' asText.
	self style: aText.

	self assertStyleOf: aText at: 8 shouldBe: #selector
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testBlockArgColonStyle [

	| aText |
	aText := 'm ^ [ :arg | arg ]' asText.
	self style: aText.

	self assertStyleOf: aText at: 7 shouldBe: #blockArgColon
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testBlockArgStyle [

	| aText |
	aText := 'm ^ [ :arg | arg ]' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 14
		and: 16
		shouldBe: #blockArg
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testBlockArgsBarStyle [

	| aText |
	aText := 'm [ :a | ^ a ]' asText.
	self style: aText.

	self assertStyleOf: aText at: 8 shouldBe: #blockArgsBar
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testBlockPatternArgStyle [

	| aText |
	aText := 'm ^ [ :arg | arg ]' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 8
		and: 10
		shouldBe: #blockPatternArg
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testBlockPatternTempVarStyle [

	| aText |
	aText := 'm ^ [ | arg | arg ]' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 9
		and: 11
		shouldBe: #blockPatternTempVar
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testBlockStyle [

	| aText |
	aText := 'm ^ [ self a and: [ self b and: [ self c and: [ self d and: [ self e and: [ self f and: [ self h and: [ self i ]]]]]]]]' asText.
	self style: aText.

	self assertStyleOf: aText at: 5 shouldBe: #block.
	self assertStyleOf: aText at: 19 shouldBe: #block1.
	self assertStyleOf: aText at: 33 shouldBe: #block2.
	self assertStyleOf: aText at: 47 shouldBe: #block3.
	self assertStyleOf: aText at: 61 shouldBe: #block4.
	self assertStyleOf: aText at: 75 shouldBe: #block5.
	self assertStyleOf: aText at: 89 shouldBe: #block6.
	self assertStyleOf: aText at: 103 shouldBe: #block7.
	self assertStyleOf: aText at: 112 shouldBe: #block7.
	self assertStyleOf: aText at: 113 shouldBe: #block6.
	self assertStyleOf: aText at: 114 shouldBe: #block5.
	self assertStyleOf: aText at: 115 shouldBe: #block4.
	self assertStyleOf: aText at: 116 shouldBe: #block3.
	self assertStyleOf: aText at: 117 shouldBe: #block2.
	self assertStyleOf: aText at: 118 shouldBe: #block1.
	self assertStyleOf: aText at: 119 shouldBe: #block
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testBlockTempBarStyle [

	| aText |
	aText := 'm [ | a | ^ a ]' asText.
	self style: aText.

	self assertStyleOf: aText at: 5 shouldBe: #blockTempBar.
	self assertStyleOf: aText at: 9 shouldBe: #blockTempBar
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testBlockTempVarStyle [

	| aText |
	aText := 'm ^ [ | arg | arg ]' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 15
		and: 17
		shouldBe: #blockTempVar
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testBracesStyle [

	| aText |
	aText := 'm ^ {1 . {2 . {3 . {4 . {5 . {6 . {7 . {8 . {9 .1}}}}}}}}' asText.
	self style: aText.

	self assertStyleOf: aText at: 10 shouldBe: #brace.
	self assertStyleOf: aText at: 15 shouldBe: #brace1.
	self assertStyleOf: aText at: 20 shouldBe: #brace2.
	self assertStyleOf: aText at: 25 shouldBe: #brace3.
	self assertStyleOf: aText at: 30 shouldBe: #brace4.
	self assertStyleOf: aText at: 35 shouldBe: #brace5.
	self assertStyleOf: aText at: 40 shouldBe: #brace6.
	self assertStyleOf: aText at: 45 shouldBe: #brace7.
	self assertStyleOf: aText at: 50 shouldBe: #brace7.
	self assertStyleOf: aText at: 51 shouldBe: #brace6.
	self assertStyleOf: aText at: 52 shouldBe: #brace5.
	self assertStyleOf: aText at: 53 shouldBe: #brace4.
	self assertStyleOf: aText at: 54 shouldBe: #brace3.
	self assertStyleOf: aText at: 55 shouldBe: #brace2.
	self assertStyleOf: aText at: 56 shouldBe: #brace1.
	self assertStyleOf: aText at: 57 shouldBe: #brace
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testCascadeSeparatorStyle [

	| aText |
	aText := 'm self a; b' asText.
	self style: aText.

	self assertStyleOf: aText at: 9 shouldBe: #cascadeSeparator
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testCharacterStyle [

	| aText |
	aText := 'm ^ $1' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 5
		and: 6
		shouldBe: #character
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testClassAfterMessageToASymbolLowercaseShouldBeColored [

	| aText |
	styler class formatIncompleteIdentifiers: true.
	aText := 'm1
		#world traceCr.
		Object new.' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 24
		and: 29
		shouldBe: #globalVar
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testClassAfterMessageToASymbolUppercaseShouldBeColored [

	| aText |
	styler class formatIncompleteIdentifiers: true.
	aText := 'm1
		#World traceCr.
		Object new.' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 24
		and: 29
		shouldBe: #globalVar
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testClassVarStyle [

	| aText |
	aText := 'm ^ Test' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 5
		and: 9
		shouldBe: #classVar
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testCommentOfVariableInParenthesis [
	"Regression test for https://github.com/pharo-project/pharo/issues/13316"

	| aText |
	aText := 'test

	| toto |
	^ (toto "this returns nil")' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 26
		and: 43
		shouldBe: #comment
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testCommentStyle [

	| aText |
	aText := 'm "return class" ^ SHRBStyleAttributionTest' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 3
		and: 16
		shouldBe: #comment
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testCommentsInLiteralArrayNode [

	| aText |
	aText := 'test
	(#(123) "comment")' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 15
		and: 23
		shouldBe: #comment
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testCommentsInLiteralValueNode [

	| aText |
	aText := 'test
	(123 "comment")' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 12
		and: 20
		shouldBe: #comment
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testCommentsInVariaousPlaces [
	"For now, the AST does not visit really well the comments. Some work is needed to do that and until it is done, this test will help to avoid regression."

	| aText |
	aText := 'test
	("comment1" (123 "comment2")) + ("comment3" (self "comment4"))' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 8
		and: 17
		shouldBe: #comment.
	self
		assertStyleOf: aText
		between: 24
		and: 33
		shouldBe: #comment.
	self
		assertStyleOf: aText
		between: 40
		and: 49
		shouldBe: #comment.
	self
		assertStyleOf: aText
		between: 57
		and: 66
		shouldBe: #comment
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testCommentsPragmaNode [

	| aText |
	aText := 'test
	<test "comment">' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 13
		and: 21
		shouldBe: #comment
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testDefaultStyle [

	| aText |
	aText := 'm thisContext senders' asText.
	self style: aText.

	self assertStyleOf: aText at: 2 shouldBe: #default
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testDeprecatedClass [

	| aText |
	aText := 'm ^ SHRBDeprecatedClassMock' asText.
	self style: aText.

	5 to: 27 do: [ :index | self assert: ((aText attributesAt: index) includes: TextEmphasis struckOut) ]
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testDeprecatedGlobalVariable [

	[
	| aText |
	Smalltalk globals at: #SHRBDeprecatedGlobalVariable put: self class.
	(Smalltalk globals lookupVar: #SHRBDeprecatedGlobalVariable) isDeprecated: true.
	aText := 'm ^ SHRBDeprecatedGlobalVariable' asText.
	self style: aText.

	5 to: 32 do: [ :index | self assert: ((aText attributesAt: index) includes: TextEmphasis struckOut) ] ] ensure: [
		Smalltalk globals removeKey: #SHRBDeprecatedGlobalVariable ifAbsent: [ "Do nothing. The test just crashed really really early." ] ]
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testDeprecatedGlobalVariableAsSymbol [

	[
	| aText |
	Smalltalk globals at: #SHRBDeprecatedGlobalVariable put: self class.
	(Smalltalk globals lookupVar: #SHRBDeprecatedGlobalVariable) isDeprecated: true.
	aText := 'm ^ #SHRBDeprecatedGlobalVariable' asText.
	self style: aText.

	5 to: 33 do: [ :index | self assert: ((aText attributesAt: index) includes: TextEmphasis struckOut) ] ] ensure: [
		Smalltalk globals removeKey: #SHRBDeprecatedGlobalVariable ifAbsent: [ "Do nothing. The test just crashed really really early." ] ]
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testErrorStyle [

	| aText |
	aText := 'm 1 ] . foo: . [ 2' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 5
		and: 5
		shouldBe: #invalid. "closing ]"

	self
		assertStyleOf: aText
		between: 9
		and: 9
		shouldBe: #invalid. "f of foo: because missing receiver"

	self
		assertStyleOf: aText
		between: 16
		and: 16
		shouldBe: #invalid. "opening ["
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testFalseStyle [

	| aText |
	aText := 'm false' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 3
		and: 7
		shouldBe: #false
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testGlobalVarStyle [

	| aText |
	aText := 'm ^ Smalltalk' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 5
		and: 11
		shouldBe: #globalVar
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testIncompleteIdentifierStyle [

	| aText |
	styler class formatIncompleteIdentifiers: true.
	aText := 'm ^ style' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 5
		and: 9
		shouldBe: #incompleteIdentifier
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testIncompleteKeywordStyle [

	| aText |
	styler class formatIncompleteIdentifiers: true.
	aText := 'm ^ self setU' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 10
		and: 13
		shouldBe: #incompleteSelector
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testInstanceVarStyle [

	| aText |
	aText := 'm ^ styler' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 5
		and: 10
		shouldBe: #instVar
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testInvalidStyle [

	| aText |
	aText := 'm ^ #(' asText.
	self style: aText.

	self assertStyleOf: aText at: 6 shouldBe: #invalid
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testKeywordStyle [

	| aText |
	aText := 'm self style: ''test''' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 8
		and: 13
		shouldBe: #selector
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testMethodTempBarStyle [

	| aText |
	aText := 'm | a | ^ a' asText.
	self style: aText.

	self assertStyleOf: aText at: 3 shouldBe: #methodTempBar.
	self assertStyleOf: aText at: 7 shouldBe: #methodTempBar
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testNilStyle [

	| aText |
	aText := 'm nil' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 3
		and: 6
		shouldBe: #nil
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testNumberStyle [

	| aText |
	aText := 'm ^ 12' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 5
		and: 6
		shouldBe: #number
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testNumberStyle2 [

	| aText |
	aText := 'm ^ -12' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 5
		and: 7
		shouldBe: #number
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testParenthesisStyle [

	| aText |
	aText := 'm ^ (1 + (2 + (3 + (4 + (5 + (6 + (7 + (8 + (9 +1))))))))' asText.
	self style: aText.

	self assertStyleOf: aText at: 10 shouldBe: #parenthesis.
	self assertStyleOf: aText at: 15 shouldBe: #parenthesis1.
	self assertStyleOf: aText at: 20 shouldBe: #parenthesis2.
	self assertStyleOf: aText at: 25 shouldBe: #parenthesis3.
	self assertStyleOf: aText at: 30 shouldBe: #parenthesis4.
	self assertStyleOf: aText at: 35 shouldBe: #parenthesis5.
	self assertStyleOf: aText at: 40 shouldBe: #parenthesis6.
	self assertStyleOf: aText at: 45 shouldBe: #parenthesis7.
	self assertStyleOf: aText at: 50 shouldBe: #parenthesis7.
	self assertStyleOf: aText at: 51 shouldBe: #parenthesis6.
	self assertStyleOf: aText at: 52 shouldBe: #parenthesis5.
	self assertStyleOf: aText at: 53 shouldBe: #parenthesis4.
	self assertStyleOf: aText at: 54 shouldBe: #parenthesis3.
	self assertStyleOf: aText at: 55 shouldBe: #parenthesis2.
	self assertStyleOf: aText at: 56 shouldBe: #parenthesis1.
	self assertStyleOf: aText at: 57 shouldBe: #parenthesis
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testParenthesisSurroundingVar [

	| aText |
	aText := 'm |var| ^ (var)' asText.
	self style: aText.

	self assertStyleOf: aText at: 11 shouldBe: #parenthesis.
	self
		assertStyleOf: aText
		between: 12
		and: 14
		shouldBe: #tempVar.
	self assertStyleOf: aText at: 15 shouldBe: #parenthesis
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testPatternArgStyle [

	| aText |
	aText := 'm: arg ^ arg' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 4
		and: 6
		shouldBe: #patternArg
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testPatternKeywordStyle [

	| aText |
	styler class formatIncompleteIdentifiers: true.
	aText := 'test: arg1 with: arg2  ^ arg1 > arg2' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 1
		and: 5
		shouldBe: #patternSelector.
	self
		assertStyleOf: aText
		between: 12
		and: 16
		shouldBe: #patternSelector
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testPatternTempVarStyle [

	| aText |
	aText := 'm | a | ^ a' asText.
	self style: aText.

	self assertStyleOf: aText at: 5 shouldBe: #patternTempVar
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testPragmaStyle [

	| aText |
	aText := 'm <test> ^ a' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 3
		and: 8
		shouldBe: #pragma
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testPragmaStyleWithCrazyPragma [

	| aText |
	aText := 'm 	<  test: #toto test2: 2 > ^ a' asText.
	self style: aText.

	self assertStyleOf: aText at: 4 shouldBe: #pragma.
	self
		assertStyleOf: aText
		between: 7
		and: 11
		shouldBe: #pragma.
	self
		assertStyleOf: aText
		between: 19
		and: 24
		shouldBe: #pragma.
	self assertStyleOf: aText at: 28 shouldBe: #pragma
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testReturnStyle [

	| aText |
	aText := 'm ^ 3' asText.
	self style: aText.

	self
		assertStyleOf: aText
		at: 3
		shouldBe: #return
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testSelfStyle [

	| aText |
	aText := 'm self' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 3
		and: 7
		shouldBe: #self
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testSharedPoolVarStyle [

	| aText |
	aText := 'm ^ PoolVar' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 5
		and: 11
		shouldBe: #poolConstant
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testStatementSeparatorStyle [

	| aText |
	aText := 'm self a. self b' asText.
	self style: aText.

	self assertStyleOf: aText at: 9 shouldBe: #statementSeparator
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testStringStyle [

	| aText |
	aText := 'm ^ ''ab''' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 5
		and: 8
		shouldBe: #string
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testStringSymbolStyle [

	| aText |
	aText := 'm ^ #''ab''' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 5
		and: 9
		shouldBe: #symbol
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testSuperStyle [

	| aText |
	aText := 'm super initialize' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 3
		and: 7
		shouldBe: #super
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testSymbolStyle [

	| aText |
	aText := 'm ^ #ab' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 5
		and: 7
		shouldBe: #symbol
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testTempVarStyle [

	| aText |
	aText := 'm | a | ^ a' asText.
	self style: aText.

	self assertStyleOf: aText at: 11 shouldBe: #tempVar
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testThisContextStyle [

	| aText |
	aText := 'm thisContext senders' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 3
		and: 12
		shouldBe: #thisContext
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testThisProcessStyle [

	| aText |
	aText := 'm thisProcess senders' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 3
		and: 12
		shouldBe: #thisProcess
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testTrueStyle [

	| aText |
	aText := 'm true' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 3
		and: 6
		shouldBe: #true
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testUnaryStyle [

	| aText |
	aText := 'm self testKeywordStyle' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 8
		and: 23
		shouldBe: #selector
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testUndefinedIdentifierStyle [

	| aText |
	styler class formatIncompleteIdentifiers: true.
	aText := 'm ^ toto' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 5
		and: 9
		shouldBe: #undefinedIdentifier
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testUndefinedKeywordStyle [

	| aText |
	styler class formatIncompleteIdentifiers: true.
	aText := 'm ^ self toto' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 10
		and: 13
		shouldBe: #undefinedSelector
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testUndefinedKeywordStyleWithSettingOff [

	| aText |
	styler class formatIncompleteIdentifiers: false.
	aText := 'm1
		^ self prin' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 13
		and: 16
		shouldBe: #undefinedSelector
]

{ #category : 'tests' }
SHRBStyleAttributionTest >> testUnfinishedCommentStyle [

	| aText |
	aText := 'm "return class ^ SHRBStyleAttributionTest' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 3
		and: 42
		shouldBe: #invalid
]
