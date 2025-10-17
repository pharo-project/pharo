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

	"Returns a textual description of the exception (based on the message instance variable). If message is nil, it returns the defaultDescription instead."

	message ifNil: [ ^ self defaultDescription ].
	^ self classSymbol printString
	  , ' is missing, and does not understand '
	  , message selector printString
]

{ #category : #accessing }
VariableNotDeclared >> variableNode [
	^ variableNode
]

{ #category : #accessing }
VariableNotDeclared >> variableNode: anObject [
	variableNode := anObject
]
