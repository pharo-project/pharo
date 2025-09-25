"
I check if a name is valid for an instance variable
"
Class {
	#name : 'ReIsValidSharedVariableName',
	#superclass : 'ReVariableNameCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
ReIsValidSharedVariableName >> check [

	| string |
	variable isString ifFalse: [
		violator := variable.
		^ false ].
	string := variable asString.
	(Symbol reservedLiterals includes: string) ifTrue: [
		violator := variable.
		^ false ].
	string isEmpty ifTrue: [
		violator := variable.
		^ false ].
	string first isUppercase ifFalse: [
		violator := variable.
		^ false ].
	(OCScanner isVariable: string) ifFalse: [
		violator := variable.
		^ false ].
	^ true
]

{ #category : 'accessing' }
ReIsValidSharedVariableName >> violationMessageOn: aStream [

	^ aStream
		  nextPutAll: violator;
		  nextPutAll:
			  ' is not a valid shared variable name (the name must start in uppercase).'
]
