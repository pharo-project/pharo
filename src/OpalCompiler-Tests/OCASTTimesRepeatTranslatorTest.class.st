Class {
	#name : #OCASTTimesRepeatTranslatorTest,
	#superclass : #OCASTSingleBlockTranslatorTest,
	#category : #'OpalCompiler-Tests-AST'
}

{ #category : #'building suites' }
OCASTTimesRepeatTranslatorTest class >> testParameters [
	^ super testParameters *
		(ParametrizedTestMatrix new
			forSelector: #optimization addOptions: { #optionInlineTimesRepeat . #optionInlineNone })
]

{ #category : #'testing - blocks - optimized' }
OCASTTimesRepeatTranslatorTest >> testTimesRepeatExecutesBlock [


	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	total := 1.
	5 timesRepeat: {argument}.
	 ^ total'
		withTemps: #('total')
		withBlock: '[ total := total  + total ]'.
	
	self assert: result equals: 2 ** 5.
]

{ #category : #'testing - blocks - optimized' }
OCASTTimesRepeatTranslatorTest >> testTimesRepeatReturnsReceiver [


	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	^ 5 timesRepeat: {argument}'
		withBlock: '[ 1+2 ]'.
	
	self assert: result equals: 5
]

{ #category : #'testing - blocks - optimized' }
OCASTTimesRepeatTranslatorTest >> testTimesRepeatWithCalculatedArguments [


	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	total := 1.
	times := 6.
	times - 1 timesRepeat: {argument}.
	 ^ total'
		withTemps: #('times' 'total')
		withBlock: '[ total := total + total ]'.
	
	self assert: result equals: 2 ** 5.
]

{ #category : #'testing - blocks - optimized' }
OCASTTimesRepeatTranslatorTest >> testTimesRepeatWithTempInBlock [

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

{ #category : #'testing - blocks - optimized' }
OCASTTimesRepeatTranslatorTest >> testTimesRepeatWithTempOutsideBlock [

	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	total := 1.
	5 timesRepeat: {argument}.
	 ^ total'
		withTemps: #('total' 'tempOutsideBlock')
		withBlock: '[
			tempOutsideBlock := total.
			total := total + tempOutsideBlock ]'.
	
	self assert: result equals: 2 ** 5.
]
