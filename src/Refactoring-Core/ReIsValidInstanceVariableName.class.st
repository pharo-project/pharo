"
I check if a name is valid for an instance variable
"
Class {
	#name : 'ReIsValidInstanceVariableName',
	#superclass : 'ReVariableNameCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
ReIsValidInstanceVariableName >> check [

	| string |
	variable isString ifFalse: [
		violator := variable.
		^ false ].
	string := variable asString.
	string isEmpty ifTrue: [
		violator := variable.
		^ false ].
	(Symbol reservedLiterals includes: string) ifTrue: [
		violator := variable.
		^ false ].
	string first isUppercase ifTrue: [
		violator := variable.
		^ false ].
	(OCScanner isVariable: string) ifFalse: [
		violator := variable.
		^ false ].
	^ true
]

{ #category : 'accessing' }
ReIsValidInstanceVariableName >> violationMessageOn: aStream [

	^ aStream
		  nextPutAll: violator;
		  nextPutAll:
			  ' the name musn''t start in uppercase or be a reserved word.'
]
