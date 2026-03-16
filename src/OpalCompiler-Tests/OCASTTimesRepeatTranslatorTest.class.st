Class {
	#name : 'OCASTTimesRepeatTranslatorTest',
	#superclass : 'OCASTSingleBlockTranslatorTest',
	#category : 'OpalCompiler-Tests-AST',
	#package : 'OpalCompiler-Tests',
	#tag : 'AST'
}

{ #category : 'building suites' }
OCASTTimesRepeatTranslatorTest class >> testParameters [
	^ super testParameters *
		(ParametrizedTestMatrix new
			forSelector: #optimization addOptions: (self inliningOptionsFor: #optionInlineTimesRepeat))
]

{ #category : 'tests' }
OCASTTimesRepeatTranslatorTest >> testNestedTimesRepeat [

	| result |

	result := self testSource: 'example
		| s |
		s := OrderedCollection new.
		
			3 timesRepeat: [ 
			  s add: #outer.
			4 timesRepeat: [ 
		     s add: #inner ] ].
		^ s'.
		
	self assert: result equals: #(#outer #inner #inner #inner #inner #outer #inner #inner #inner #inner #outer #inner #inner #inner #inner) asOrderedCollection.
]

{ #category : 'tests' }
OCASTTimesRepeatTranslatorTest >> testSequencedTimesRepeat [

	| result |

	result := self testSource: 'example
		| s |
		s := OrderedCollection new.
		
			3 timesRepeat: [ 
			  s add: #first ].
			4 timesRepeat: [ 
		     s add: #second ].
		^ s'.
		
	self assert: result equals: #(#first #first #first #second #second #second #second) asOrderedCollection.
]

{ #category : 'tests - blocks - optimized' }
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

	self assert: result equals: 2 ** 5
]

{ #category : 'tests - blocks - optimized' }
OCASTTimesRepeatTranslatorTest >> testTimesRepeatReturnsReceiver [


	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	^ 5 timesRepeat: {argument}'
		withBlock: '[ 1+2 ]'.

	self assert: result equals: 5
]

{ #category : 'tests - blocks - optimized' }
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

	self assert: result equals: 2 ** 5
]

{ #category : 'tests - blocks - optimized' }
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

	self assert: result equals: 2 ** 5
]

{ #category : 'tests - blocks - optimized' }
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

	self assert: result equals: 2 ** 5
]
