"
SUnit tests for true/True class
"
Class {
	#name : #TrueTest,
	#superclass : #ClassTestCase,
	#category : #'Kernel-Tests-Objects'
}

{ #category : #'tests - logical operations' }
TrueTest >> testAND [

	self assert: (true & true).
	self deny: (true & false).
]

{ #category : #'tests - controlling' }
TrueTest >> testAnd [
	
	self assert: ((true and: ['alternativeBlock']) = 'alternativeBlock'). 
]

{ #category : #'tests - converting' }
TrueTest >> testAsBit [

       self assert: (true asBit = 1).
]

{ #category : #'tests - controlling' }
TrueTest >> testIfFalse [
	
	self assert: ((true ifFalse: ['alternativeBlock']) isNil). 
]

{ #category : #'tests - controlling' }
TrueTest >> testIfFalseIfTrue [

	self assert: (true ifFalse: ['falseAlternativeBlock'] 
                      ifTrue: ['trueAlternativeBlock']) = 'trueAlternativeBlock'. 
]

{ #category : #'tests - controlling' }
TrueTest >> testIfTrue [

	self assert: ((true ifTrue: ['alternativeBlock']) = 'alternativeBlock'). 
]

{ #category : #'tests - controlling' }
TrueTest >> testIfTrueIfFalse [

	self assert: (true ifTrue: ['trueAlternativeBlock'] 
                      ifFalse: ['falseAlternativeBlock']) = 'trueAlternativeBlock'. 
]

{ #category : #'tests - instance creation' }
TrueTest >> testNew [

	self should: [True new] raise: self defaultTestError 
]

{ #category : #'tests - logical operations' }
TrueTest >> testNot [

	self deny: (true not).
]

{ #category : #'tests - logical operations' }
TrueTest >> testOR [

	self assert: (true | true).
	self assert: (true | false).
]

{ #category : #'tests - controlling' }
TrueTest >> testOr [

	self assert: (true or: [ 'alternativeBlock' ])
]

{ #category : #'tests - printing' }
TrueTest >> testPrintOn [

	self assert: (String streamContents: [:stream | true printOn: stream]) = 'true'. 
]

{ #category : #'tests - logical operations' }
TrueTest >> testXor [
	self assert: (true xor: true) = false.
	self assert: (true xor: false) = true.
	self assert: (true xor: [true]) = false.
	self assert: (true xor: [false]) = true.
]
