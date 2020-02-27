"
SUnit tests for false/False class
"
Class {
	#name : #FalseTest,
	#superclass : #ClassTestCase,
	#category : #'Kernel-Tests-Objects'
}

{ #category : #'tests - logical operations' }
FalseTest >> testAND [

	self deny: (false & true).
	self deny: (false & false).
]

{ #category : #'tests - controlling' }
FalseTest >> testAnd [

	self deny: (false and: ['alternativeBlock']).
]

{ #category : #'tests - converting' }
FalseTest >> testAsBit [

	self assert: false asBit equals: 0
]

{ #category : #'tests - controlling' }
FalseTest >> testIfFalse [
	self assert: (false ifFalse: [ 'alternativeBlock' ]) equals: 'alternativeBlock'
]

{ #category : #'tests - controlling' }
FalseTest >> testIfFalseIfTrue [
	self assert: (false ifFalse: [ 'falseAlternativeBlock' ] ifTrue: [ 'trueAlternativeBlock' ]) equals: 'falseAlternativeBlock'
]

{ #category : #'tests - controlling' }
FalseTest >> testIfTrue [
	self
		assert:
			(false
				ifTrue: [ 'alternativeBlock' ]) isNil
]

{ #category : #'tests - controlling' }
FalseTest >> testIfTrueIfFalse [
	self assert: (false ifTrue: [ 'trueAlternativeBlock' ] ifFalse: [ 'falseAlternativeBlock' ]) equals: 'falseAlternativeBlock'
]

{ #category : #'tests - instance creation' }
FalseTest >> testNew [

	self should: [False new] raise: self defaultTestError 
]

{ #category : #'tests - logical operations' }
FalseTest >> testNot [

	self assert: (false not).
]

{ #category : #'tests - logical operations' }
FalseTest >> testOR [

	self assert: (false | true).
	self deny: (false | false).
]

{ #category : #'tests - controlling' }
FalseTest >> testOr [
	self assert: (false or: [ 'alternativeBlock' ]) equals: 'alternativeBlock'
]

{ #category : #'tests - printing' }
FalseTest >> testPrintOn [
	self assert: (String streamContents: [ :stream | false printOn: stream ]) equals: 'false'
]

{ #category : #'tests - logical operations' }
FalseTest >> testXor [
	self assert: (false xor: true) equals: true.
	self assert: (false xor: false) equals: false.
	self assert: (false xor: [ true ]) equals: true.
	self assert: (false xor: [ false ]) equals: false
]
