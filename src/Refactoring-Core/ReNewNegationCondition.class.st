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
	subjects := condition candidates
]

{ #category : 'accessing' }
ReNewNegationCondition >> errorString [ 

	^ condition errorString
]

{ #category : 'accessing' }
ReNewNegationCondition >> nonViolators [

	^ subjects difference: self violators
]

{ #category : 'displaying' }
ReNewNegationCondition >> violationMessageOn: aWriteStream [
	| method re message message2 |
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
	message2 := re copy: message contents replacingMatchesWith: ' '.
	message2 == message ifTrue: [ message2 := 'NOT ' + message ].
	aWriteStream nextPutAll: message2
]

{ #category : 'accessing' }
ReNewNegationCondition >> violators [

	^ condition nonViolators
]
