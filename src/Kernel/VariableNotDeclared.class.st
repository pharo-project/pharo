"
This exception is provided to support doesNotUnderstand: on missing classes (Undeclared variables)
"
Class {
	#name : #VariableNotDeclared,
	#superclass : #MessageNotUnderstood,
	#instVars : [
		'variableNode'
	],
	#category : #'Kernel-Exceptions'
}

{ #category : #accessing }
VariableNotDeclared >> classSymbol [
	^ self variableNode name
]

{ #category : #accessing }
VariableNotDeclared >> description [
	"Returns a textual description of the exception, built using the message field if possible. If message is Nil, it uses the superclass description instead (as default)."
	
	message ifNil: [^super description].

	^self messageBasedDescription
]

{ #category : #accessing }
VariableNotDeclared >> messageBasedDescription [
	"Returns a textual representation of the exception based on the message field"	
	self classSymbol printString
		, ' is missing, and does not understand ', message selector printString
]

{ #category : #accessing }
VariableNotDeclared >> variableNode [
	^ variableNode
]

{ #category : #accessing }
VariableNotDeclared >> variableNode: anObject [
	variableNode := anObject
]
