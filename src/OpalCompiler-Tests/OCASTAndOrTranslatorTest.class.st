Class {
	#name : #OCASTAndOrTranslatorTest,
	#superclass : #OCASTSingleBlockTranslatorTest,
	#category : #'OpalCompiler-Tests-AST'
}

{ #category : #'building suites' }
OCASTAndOrTranslatorTest class >> testParameters [
	^ super testParameters *
		(ParametrizedTestMatrix new
			forSelector: #optimization addOptions: { #optionInlineAndOr . #optionInlineNone })
]

{ #category : #'testing - andor' }
OCASTAndOrTranslatorTest >> testAndWithLeftFalseShortcircuitsRight [

	self
		testComposedExample: 'example
	{definition}.
	false and: {argument}'
		withBlock: '[ result := 17 ]'.
	self assert: instance result equals: nil
]

{ #category : #'testing - andor' }
OCASTAndOrTranslatorTest >> testAndWithLeftTrueEvaluatesRight [

	self
		testComposedExample: 'example
	{definition}.
	true and: {argument}'
		withBlock: '[ result := 17 ]'.
	self assert: instance result equals: 17.
]

{ #category : #'testing - andor' }
OCASTAndOrTranslatorTest >> testFalseAndAnythingReturnsFalse [

	| result |
	result := self
		testComposedExample: 'example: anything
	{definition}.
	^ false and: {argument}'
		withBlock: '[ anything ]'
		withArguments: #(17).
	self deny: result
]

{ #category : #'testing - andor' }
OCASTAndOrTranslatorTest >> testFalseOrAnythingReturnsAnything [

	| result |
	result := self
		testComposedExample: 'example: anything
	{definition}.
	^ false or: {argument}'
		withBlock: '[ anything ]'
		withArguments: #(17).
	self assert: result equals: 17
]

{ #category : #'testing - andor' }
OCASTAndOrTranslatorTest >> testOrWithLeftFalseEvaluatesRight [

	self
		testComposedExample: 'example
	{definition}.
	false or: {argument}'
		withBlock: '[ result := 17 ]'.
	self assert: instance result equals: 17.
]

{ #category : #'testing - andor' }
OCASTAndOrTranslatorTest >> testOrWithLeftTrueShortcircuitsRight [

	self
		testComposedExample: 'example
	{definition}.
	true or: {argument}'
		withBlock: '[ result := 17 ]'.
	self assert: instance result equals: nil
]

{ #category : #'testing - andor' }
OCASTAndOrTranslatorTest >> testTrueAndAnythingReturnsAnything [

	| result |
	result := self
		testComposedExample: 'example: anything
	{definition}.
	^ true and: {argument}'
		withBlock: '[ anything ]'
		withArguments: #(17).
	self assert: result equals: 17
]

{ #category : #'testing - andor' }
OCASTAndOrTranslatorTest >> testTrueOrAnythingReturnsTrue [

	| result |
	result := self
		testComposedExample: 'example: anything
	{definition}.
	^ true or: {argument}'
		withBlock: '[ anything ]'
		withArguments: #(17).
	self assert: result
]
