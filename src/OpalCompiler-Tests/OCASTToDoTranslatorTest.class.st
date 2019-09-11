Class {
	#name : #OCASTToDoTranslatorTest,
	#superclass : #OCASTSingleBlockTranslatorTest,
	#category : #'OpalCompiler-Tests-AST'
}

{ #category : #'building suites' }
OCASTToDoTranslatorTest class >> testParameters [
	^ super testParameters *
		(ParametrizedTestMatrix new
			forSelector: #optimization addOptions: { #optionInlineToDo . #optionInlineNone })
]

{ #category : #'testing - blocks - optimized' }
OCASTToDoTranslatorTest >> testToDoExecutesBlock [


	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	total := 0.
	1 to: 5 do: {argument}.
	 ^ total'
		withTemps: #('total')
		withBlock: '[ :index | total := total + index ]'.
	
	self assert: result equals: 15.
]

{ #category : #'testing - blocks - optimized' }
OCASTToDoTranslatorTest >> testToDoReturnsReceiver [


	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	^ 1 to: 5 do: {argument}'
		withBlock: '[ :index | 1+2 ]'.
	
	self assert: result equals: 1.
]

{ #category : #'testing - blocks - optimized' }
OCASTToDoTranslatorTest >> testToDoWithCalculatedArguments [


	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	total := 0.
	begin := 1.
	end := 6.
	begin to: end - 1 do: {argument}.
	 ^ total'
		withTemps: #('begin' 'end' 'total')
		withBlock: '[ :index | total := total + index ]'.
	
	self assert: result equals: 15.
]

{ #category : #'testing - blocks - optimized' }
OCASTToDoTranslatorTest >> testToDoWithTempInBlock [

	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	total := 0.
	begin := 1.
	end := 6.
	begin to: end - 1 do: {argument}.
	 ^ total'
		withTemps: #('begin' 'end' 'total')
		withBlock: '[ :index | | tempInBlock | 
			tempInBlock := index.
			total := total + tempInBlock ]'.
	
	self assert: result equals: 15.
]

{ #category : #'testing - blocks - optimized' }
OCASTToDoTranslatorTest >> testToDoWithTempOutsideBlock [

	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	total := 0.
	begin := 1.
	end := 6.
	begin to: end - 1 do: {argument}.
	 ^ total'
		withTemps: #('begin' 'end' 'total' 'tempOutsideBlock')
		withBlock: '[ :index |
			tempOutsideBlock := index.
			total := total + tempOutsideBlock ]'.
	
	self assert: result equals: 15.
]
