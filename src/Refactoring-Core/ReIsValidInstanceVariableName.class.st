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
	name isString ifFalse: [
		violator := name.
		^ false ].
	string := name asString.
	string isEmpty ifTrue: [
		violator := name.
		^ false ].
	(Symbol reservedLiterals includes: string) ifTrue: [
		violator := name.
		^ false ].
	string first isUppercase ifTrue: [
		violator := name.
		^ false ].
	(OCScanner isVariable: string) ifFalse: [
		violator := name.
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
