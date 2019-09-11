Class {
	#name : #OCASTSingleBranchConditionalTranslatorTest,
	#superclass : #OCASTSingleBlockTranslatorTest,
	#category : #'OpalCompiler-Tests-AST'
}

{ #category : #'building suites' }
OCASTSingleBranchConditionalTranslatorTest class >> testParameters [
	^ super testParameters *
		(ParametrizedTestMatrix new
			forSelector: #optimization addOptions: { #optionInlineIf . #optionInlineIfNil . #optionInlineNone })
]

{ #category : #'testing-ifFalse' }
OCASTSingleBranchConditionalTranslatorTest >> testFalseIfFalseEvaluatesBlock [

	self
		testComposedExample: 'example
	{definition}.
	^false ifFalse: {argument}'
		withBlock: '[iVar := 17]'.
	self assert: instance iVar equals: 17
]

{ #category : #'testing-ifFalse' }
OCASTSingleBranchConditionalTranslatorTest >> testFalseIfFalseEvaluatesBlockWithTempInsideBlock [

	self
		testComposedExample: 'example
	{definition}.
	^false ifFalse: {argument}'
		withBlock: '[ | temp |
			temp := 17.
			iVar := temp ]'.
	self assert: instance iVar equals: 17
]

{ #category : #'testing-ifFalse' }
OCASTSingleBranchConditionalTranslatorTest >> testFalseIfFalseEvaluatesBlockWithTempOutsideBlock [

	self
		testComposedExample: 'example
	{definition}.
	^false ifFalse: {argument}'
		withTemps: #( 'temp' )
		withBlock: '[
			temp := 17.
			iVar := temp ]'.
	self assert: instance iVar equals: 17
]

{ #category : #'testing-ifFalse' }
OCASTSingleBranchConditionalTranslatorTest >> testFalseIfFalseReturnsBlockValue [

	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	^false ifFalse: {argument}'
		withBlock: '[1]'.
	self assert: result equals: 1
]

{ #category : #'testing-ifFalse' }
OCASTSingleBranchConditionalTranslatorTest >> testFalseIfFalseReturnsBlockValueWithTempInsideBlock [

	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	^false ifFalse: {argument}'
		withBlock: '[ | temp | 
			temp := 1.
			temp ]'.
	self assert: result equals: 1
]

{ #category : #'testing-ifFalse' }
OCASTSingleBranchConditionalTranslatorTest >> testFalseIfFalseReturnsBlockValueWithTempOutsideBlock [

	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	^false ifFalse: {argument}'
		withTemps: #( 'temp' )
		withBlock: '[ 
			temp := 1.
			temp ]'.
	self assert: result equals: 1
]

{ #category : #'testing-ifTrue' }
OCASTSingleBranchConditionalTranslatorTest >> testFalseIfTrueDoesNotEvaluateBlock [

	self
		testComposedExample: 'example
	{definition}.
	^false ifTrue: {argument}'
		withBlock: '[iVar := 17]'.
	self assert: instance iVar equals: nil
]

{ #category : #'testing-ifTrue' }
OCASTSingleBranchConditionalTranslatorTest >> testFalseIfTrueReturnsNil [

	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	^false ifTrue: {argument}'
		withBlock: '[1]'.
	self assert: result equals: nil
]

{ #category : #'testing-ifNil' }
OCASTSingleBranchConditionalTranslatorTest >> testNilIfNilEvaluatesBlock [

	self
		testComposedExample: 'example
	{definition}.
	^nil ifNil: {argument}'
		withBlock: '[ iVar := 17 ]'.
	self assert: instance iVar equals: 17
]

{ #category : #'testing-ifNil' }
OCASTSingleBranchConditionalTranslatorTest >> testNilIfNilReturnsBlockValue [
	| result |
	
	result := self
		testComposedExample: 'example
	{definition}.
	^nil ifNil: {argument}'
		withBlock: '[ iVar := 17 ]'.
	self assert: result equals: 17
]

{ #category : #'testing-ifNotNil' }
OCASTSingleBranchConditionalTranslatorTest >> testNilIfNotNilDoesNotEvaluateBlock [

	self
		shouldnt: [
			self
				testComposedExample: 'example
			{definition}.
			^ nil ifNotNil: {argument}'
				withBlock: '[ self error ]' ]
		raise: Error
]

{ #category : #'testing-ifNotNil' }
OCASTSingleBranchConditionalTranslatorTest >> testNilIfNotNilReturnsNil [
	| result |
	
	result := self
		testComposedExample: 'example
	{definition}.
	^nil ifNotNil: {argument}'
		withBlock: '[ self error ]'.
	self assert: result equals: nil
]

{ #category : #'testing-ifNotNil' }
OCASTSingleBranchConditionalTranslatorTest >> testNilIfNotNilWithArgumentDoesNotEvaluateBlock [
	self shouldnt: [ 
		self
			testComposedExample: 'example
		{definition}.
		^nil ifNotNil: {argument}'
			withBlock: '[ :arg | self error ]' ] raise: Error
]

{ #category : #'testing-ifNotNil' }
OCASTSingleBranchConditionalTranslatorTest >> testNilIfNotNilWithArgumentReturnsNil [
	| result |
	result := self
			testComposedExample: 'example
		{definition}.
		^nil ifNotNil: {argument}'
			withBlock: '[ :arg | self error ]'.
	self assert: result equals: nil
]

{ #category : #'testing-ifNil' }
OCASTSingleBranchConditionalTranslatorTest >> testNotNilIfNilDoesNotEvaluateBlock [

	self
		shouldnt: [ 
			self
				testComposedExample: 'example
			{definition}.
			^1 ifNil: {argument}'
				withBlock: '[ self error ]' ]
		raise: Error
]

{ #category : #'testing-ifNil' }
OCASTSingleBranchConditionalTranslatorTest >> testNotNilIfNilReturnsReceiver [
	| result |
	result := self
			testComposedExample: 'example
		{definition}.
		^1 ifNil: {argument}'
			withBlock: '[ self error ]'.
	self assert: result equals: 1
]

{ #category : #'testing-ifNotNil' }
OCASTSingleBranchConditionalTranslatorTest >> testNotNilIfNotNilEvaluatesBlock [

	self
		testComposedExample: 'example
		{definition}.
		^1 ifNotNil: {argument}'
		withBlock: '[ iVar := 17 ]'.

	self assert: instance iVar equals: 17
]

{ #category : #'testing-ifNotNil' }
OCASTSingleBranchConditionalTranslatorTest >> testNotNilIfNotNilReturnsBlockValue [
	| result |
	result := self
		testComposedExample: 'example
		{definition}.
		^1 ifNotNil: {argument}'
		withBlock: '[ iVar := 17 ]'.
	self assert: result equals: 17
]

{ #category : #'testing-ifNotNil' }
OCASTSingleBranchConditionalTranslatorTest >> testNotNilIfNotNilWithArgumentEvaluatesBlock [

	self
		testComposedExample: 'example
		{definition}.
		^1 ifNotNil: {argument}'
		withBlock: '[ :arg | iVar := arg ]'.

	self assert: instance iVar equals: 1
]

{ #category : #'testing-ifNotNil' }
OCASTSingleBranchConditionalTranslatorTest >> testNotNilIfNotNilWithArgumentReturnsBlockValue [
	| result |
	result := 	self
		testComposedExample: 'example
		{definition}.
		^1 ifNotNil: {argument}'
		withBlock: '[ :arg | iVar := arg ]'.
	self assert: result equals: 1
]

{ #category : #'testing-ifFalse' }
OCASTSingleBranchConditionalTranslatorTest >> testTrueIfFalseDoesNotEvaluateBlock [

	self
		testComposedExample: 'example
	{definition}.
	^true ifFalse: {argument}'
		withBlock: '[iVar := 17]'.

	self assert: instance iVar equals: nil
]

{ #category : #'testing-ifFalse' }
OCASTSingleBranchConditionalTranslatorTest >> testTrueIfFalseReturnsNil [

	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	^true ifFalse: {argument}'
		withBlock: '[ 1 ]'.
	self assert: result equals: nil
]

{ #category : #'testing-ifTrue' }
OCASTSingleBranchConditionalTranslatorTest >> testTrueIfTrueEvaluateBlock [

	self
		testComposedExample: 'example
	{definition}.
	^true ifTrue: {argument}'
		withBlock: '[ iVar := 17 ]'.
	self assert: instance iVar equals: 17
]

{ #category : #'testing-ifTrue' }
OCASTSingleBranchConditionalTranslatorTest >> testTrueIfTrueEvaluateBlockWithTempInsideBlock [

	self
		testComposedExample: 'example
	{definition}.
	^true ifTrue: {argument}'
		withBlock: '[ | temp |
			temp := 17.
			iVar := temp ]'.
	self assert: instance iVar equals: 17
]

{ #category : #'testing-ifTrue' }
OCASTSingleBranchConditionalTranslatorTest >> testTrueIfTrueEvaluateBlockWithTempOutsideBlock [

	self
		testComposedExample: 'example
	{definition}.
	^true ifTrue: {argument}'
		withTemps: #( 'temp' )
		withBlock: '[
			temp := 17.
			iVar := temp ]'.
	self assert: instance iVar equals: 17
]

{ #category : #'testing-ifTrue' }
OCASTSingleBranchConditionalTranslatorTest >> testTrueIfTrueReturnsBlockValue [

	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	^true ifTrue: {argument}'
		withBlock: '[ 1 ]'.
	self assert: result equals: 1
]

{ #category : #'testing-ifTrue' }
OCASTSingleBranchConditionalTranslatorTest >> testTrueIfTrueReturnsBlockValueWithTempInsideBlock [

	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	^true ifTrue: {argument}'
		withBlock: '[ | temp |
			temp := 1.
			temp ]'.
	self assert: result equals: 1
]

{ #category : #'testing-ifTrue' }
OCASTSingleBranchConditionalTranslatorTest >> testTrueIfTrueReturnsBlockValueWithTempOutsideBlock [

	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	^true ifTrue: {argument}'
		withTemps: #( 'temp' )
		withBlock: '[
			temp := 1.
			temp ]'.
	self assert: result equals: 1
]
