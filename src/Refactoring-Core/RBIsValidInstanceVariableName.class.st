"
I check if a name is valid for an instance variable
"
Class {
	#name : 'RBIsValidInstanceVariableName',
	#superclass : 'RBVariableNameCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
RBIsValidInstanceVariableName >> check [

	| string |
	name isString ifFalse: [
		violator := name.
		^ false ].
	string := name asString.
	string isEmpty ifTrue: [
		violator := name.
		^ false ].
	(self class reservedNames includes: string) ifTrue: [
		violator := name.
		^ false ].
	string first isUppercase ifTrue: [
		violator := name.
		^ false ].
	(RBScanner isVariable: string) ifFalse: [
		violator := name.
		^ false ].
	^ true
]

{ #category : 'accessing' }
RBIsValidInstanceVariableName >> violationMessageOn: aStream [

	^ aStream
		  nextPutAll: violator;
		  nextPutAll:
			  ' the name musn''t start in uppercase or be a reserved word.'
]
