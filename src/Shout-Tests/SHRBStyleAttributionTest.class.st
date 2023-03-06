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
		noPattern: false ;
		options:  #(+ optionParseErrors + optionSkipSemanticWarnings);
		parse.
	styler style: aText ast: ast.

	^ ast
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
