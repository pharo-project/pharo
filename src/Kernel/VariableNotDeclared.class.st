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
VariableNotDeclared >> smartDescription [
	
	message ifNil: [^self description].

	^self classSymbol printString
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
