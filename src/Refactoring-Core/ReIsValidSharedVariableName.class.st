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
	name isString ifFalse: [
		violator := name.
		^ false ].
	string := name asString.
	(Symbol reservedLiterals includes: string) ifTrue: [
		violator := name.
		^ false ].
	string isEmpty ifTrue: [
		violator := name.
		^ false ].
	string first isUppercase ifFalse: [
		violator := name.
		^ false ].
	(OCScanner isVariable: string) ifFalse: [
		violator := name.
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
