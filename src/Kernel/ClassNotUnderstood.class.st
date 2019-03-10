"
This exception is provided to support doesNotUnderstand: on missing classes (Undeclared variables)
"
Class {
	#name : #ClassNotUnderstood,
	#superclass : #MessageNotUnderstood,
	#instVars : [
		'variableNode'
	],
	#category : #'Kernel-Exceptions'
}

{ #category : #accessing }
ClassNotUnderstood >> classSymbol [
	^ self variableNode name
]

{ #category : #accessing }
ClassNotUnderstood >> smartDescription [
	
	message ifNil: [^self description].

	^self classSymbol printString
		, ' is missing, and does not understand ', message selector printString
]

{ #category : #accessing }
ClassNotUnderstood >> variableNode [
	^ variableNode
]

{ #category : #accessing }
ClassNotUnderstood >> variableNode: anObject [
	variableNode := anObject
]
