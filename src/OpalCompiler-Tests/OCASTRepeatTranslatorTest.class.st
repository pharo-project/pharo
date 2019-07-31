Class {
	#name : #OCASTRepeatTranslatorTest,
	#superclass : #OCASTSingleBlockTranslatorTest,
	#category : #'OpalCompiler-Tests-AST'
}

{ #category : #'building suites' }
OCASTRepeatTranslatorTest class >> testParameters [
	^ super testParameters *
		(ParametrizedTestMatrix new
			forSelector: #optimization addOptions: { #optionInlineRepeat . #optionInlineNone })
]

{ #category : #'testing - blocks - optimized' }
OCASTRepeatTranslatorTest >> testRepeatExecutesBlock [


	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	total := 1.
	{argument} repeat.
	 ^ total'
		withTemps: #('total')
		withBlock: '[ total := total  + 1. total > 5 ifTrue: [ ^ total ] ]'.
	
	self assert: result equals: 6.
]

{ #category : #'testing - blocks - optimized' }
OCASTRepeatTranslatorTest >> testRepeatWithTempInBlock [

	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	total := 1.
	{argument} repeat.
	 ^ total'
		withTemps: #('total')
		withBlock: '[ | tempInBlock | 
			tempInBlock := total.
			total := tempInBlock + 1.
			total > 5 ifTrue: [ ^ total ] ]'.
	
	self assert: result equals: 6.
]

{ #category : #'testing - blocks - optimized' }
OCASTRepeatTranslatorTest >> testTimesRepeatWithTempInBlock [

	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	total := 1.
	5 timesRepeat: {argument}.
	 ^ total'
		withTemps: #('total')
		withBlock: '[ | tempInBlock | 
			tempInBlock := total.
			total := total + tempInBlock ]'.
	
	self assert: result equals: 2 ** 5.
]
