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
SHRBStyleAttributionTest >> testSelfStyle [

	| aText |
	aText := 'm self initialize' asText.
	self style: aText.

	self
		assertStyleOf: aText
		between: 3
		and: 6
		shouldBe: #self
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
