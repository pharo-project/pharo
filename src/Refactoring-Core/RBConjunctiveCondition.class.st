"
I am a refactoring condition combining two other conditions by a boolean AND.

I am created by sending the binary message #& to another condition (left) with another condition (right) as its argument.

Checking this conditions holds true, if my left and my right condition is true. 
"
Class {
	#name : #RBConjunctiveCondition,
	#superclass : #RBAbstractCondition,
	#instVars : [
		'left',
		'right',
		'failed'
	],
	#category : #'Refactoring-Core-Conditions'
}

{ #category : #checking }
RBConjunctiveCondition >> check [
	left check
		ifFalse: 
			[failed := #leftFailed.
			^false].
	right check
		ifFalse: 
			[failed := #rightFailed.
			^false].
	^true
]

{ #category : #private }
RBConjunctiveCondition >> errorBlockFor: aBoolean [ 
	^aBoolean
		ifTrue: [nil]
		ifFalse: 
			[failed = #leftFailed ifTrue: [left errorBlock] ifFalse: [right errorBlock]]
]

{ #category : #private }
RBConjunctiveCondition >> errorMacro [

	^ errorMacro ifNil: [ self longMacro ] ifNotNil: [ super errorMacro ]
]

{ #category : #private }
RBConjunctiveCondition >> errorStringFor: aBoolean [ 
	^aBoolean
		ifTrue: [self neitherFailed]
		ifFalse: [self perform: failed]
]

{ #category : #initialization }
RBConjunctiveCondition >> left: aCondition right: aCondition2 [ 
	left := aCondition.
	right := aCondition2.
	failed := #unknownFailed
]

{ #category : #private }
RBConjunctiveCondition >> leftFailed [
	^left errorStringFor: false
]

{ #category : #private }
RBConjunctiveCondition >> longMacro [
	^'(' , left errorMacro , ') <1?AND:OR> (' , right errorMacro , ')'
]

{ #category : #private }
RBConjunctiveCondition >> neitherFailed [
	^(left errorStringFor: true) , ' AND ' , (right errorStringFor: true)
]

{ #category : #printing }
RBConjunctiveCondition >> printOn: aStream [ 
	aStream 
		print: left;
		nextPutAll: ' & ';
		print: right

]

{ #category : #private }
RBConjunctiveCondition >> rightFailed [
	^right errorStringFor: false
]

{ #category : #private }
RBConjunctiveCondition >> unknownFailed [
	^(left errorStringFor: false) , ' OR ' , (right errorStringFor: false)
]
