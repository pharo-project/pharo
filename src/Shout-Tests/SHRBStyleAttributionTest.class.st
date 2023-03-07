Class {
	#name : #SHRBStyleAttributionTest,
	#superclass : #TestCase,
	#instVars : [
		'styler'
	],
	#category : #'Shout-Tests-Styling'
}

{ #category : #asserting }
SHRBStyleAttributionTest >> assertStyleOf: aText between: min and: max shouldBe: aStyleSymbol [

	min to: max do: [ :index | self assert: (aText attributesAt: index) anyOne name equals: aStyleSymbol ]
]

{ #category : #running }
SHRBStyleAttributionTest >> setUp [

	super setUp.
	styler := SHRBMockTextStyler new.
]

{ #category : #accessing }
SHRBStyleAttributionTest >> style: aText [

	| ast |
	ast := self class compiler
		       source: aText asString;
		       noPattern: false;
		       class: self class;
		       options: #( + optionParseErrors + optionSkipSemanticWarnings );
		       parse.
	styler style: aText ast: ast.

	^ ast
]

{ #category : #tests }
SHRBStyleAttributionTest >> testAssignmentStyle [

	| aText |
	aText := 'm ^ styler := 2' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 5
		and: 10
		shouldBe: #instVar.
	self
		assertStyleOf: aText
		between: 15
		and: 15
		shouldBe: #number
]

{ #category : #tests }
SHRBStyleAttributionTest >> testBinaryStyle [

	| aText |
	aText := 'm self / 3' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 8
		and: 8
		shouldBe: #keyword
]

{ #category : #tests }
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

{ #category : #tests }
SHRBStyleAttributionTest >> testClassVarStyle [

	| aText |
	aText := 'm ^ SHRBStyleAttributionTest' asText.
	self style: aText.

	"For this one I think it should return #classVar"
	self
		assertStyleOf: aText
		between: 5
		and: 28
		shouldBe: #globalVar
]

{ #category : #tests }
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

{ #category : #tests }
SHRBStyleAttributionTest >> testDefaultStyle [

	| aText |
	aText := 'm thisContext senders' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 2
		and: 2
		shouldBe: #default
]

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
SHRBStyleAttributionTest >> testInvalidStyle [

	| aText |
	aText := 'm ^ #(' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 6
		and: 6
		shouldBe: #invalid
]

{ #category : #tests }
SHRBStyleAttributionTest >> testKeywordStyle [

	| aText |
	aText := 'm self style: ''test''' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 8
		and: 13
		shouldBe: #keyword
]

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
SHRBStyleAttributionTest >> testReturnStyle [

	| aText |
	aText := 'm ^ 3' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 3
		and: 3
		shouldBe: #return
]

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
SHRBStyleAttributionTest >> testTempVarStyle [

	| aText |
	aText := 'm | a | ^ a' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 11
		and: 11
		shouldBe: #tempVar
]

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
SHRBStyleAttributionTest >> testUnaryStyle [

	| aText |
	aText := 'm self testKeywordStyle' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 8
		and: 23
		shouldBe: #keyword
]

{ #category : #tests }
SHRBStyleAttributionTest >> testUndefinedIdentifierStyle [

	| aText |
	thisContext.
	aText := 'm ^ toto' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 5
		and: 9
		shouldBe: #undefinedIdentifier
]

{ #category : #tests }
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
