Class {
	#name : 'ReNewNegationCondition',
	#superclass : 'RBNewAbstractCondition',
	#instVars : [
		'condition'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'instance creation' }
ReNewNegationCondition class >> on: aRBInstanceVariableHasReferences [ 
	^ self new condition: aRBInstanceVariableHasReferences ; yourself.
]

{ #category : 'checking' }
ReNewNegationCondition >> check [
	^condition check not
]

{ #category : 'accessing' }
ReNewNegationCondition >> condition: aCondition [
	condition := aCondition.
]

{ #category : 'accessing' }
ReNewNegationCondition >> errorString [ 

	^ String streamContents: [ :aStream |
		self violationMessageOn: aStream ]
]

{ #category : 'accessing' }
ReNewNegationCondition >> nonViolators [
	^ condition violators
]

{ #category : 'logical operations' }
ReNewNegationCondition >> not [
	^ condition
]

{ #category : 'displaying' }
ReNewNegationCondition >> violationMessageOn: aWriteStream [
	| method re message |
	"If my condition is a negation, return its condition violation message"
	condition class == self class 
		ifTrue: [ ^ condition condition violationMessageOn: aWriteStream ].

	"Otherwise, send the method defined in the condition to self.
	In this way, violators return nonViolators"
	method := condition class lookupSelector: #violationMessageOn:.
	message := String streamContents: [:s | 
		method valueWithReceiver: self arguments: { s }].

	"Remove 'not' apparitions from the message"
	re := '[ ]+not[ ]*' asRegex.
	message := re copy: message contents replacingMatchesWith: ' '.
	aWriteStream nextPutAll: message

]

{ #category : 'accessing' }
ReNewNegationCondition >> violators [
	"This is probably wrong because of the negation"
	^ condition nonViolators
]
