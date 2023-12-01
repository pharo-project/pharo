"
I check if a name is valid for an instance variable
"
Class {
	#name : 'RBIsValidSharedVariableName',
	#superclass : 'RBVariableNameCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
RBIsValidSharedVariableName >> check [

	| string |
	name isString ifFalse: [
		violator := name.
		^ false ].
	string := name asString.
	(self class reservedNames includes: string) ifTrue: [
		violator := name.
		^ false ].
	string isEmpty ifTrue: [
		violator := name.
		^ false ].
	string first isUppercase ifFalse: [
		violator := name.
		^ false ].
	(RBScanner isVariable: string) ifFalse: [
		violator := name.
		^ false ].
	^ true
]

{ #category : 'accessing' }
RBIsValidSharedVariableName >> violationMessageOn: aStream [

	^ aStream
		  nextPutAll: violator;
		  nextPutAll:
			  ' is not a valid shared variable name (the name must start in uppercase).'
]
