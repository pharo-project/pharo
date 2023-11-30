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
		violators := { name }.
		^ false ].
	string := name asString.
	(self class reservedNames includes: string) ifTrue: [
		violators := { name }.
		^ false ].
	string isEmpty ifTrue: [
		violators := { name }.
		^ false ].
	string first isUppercase ifFalse: [
		violators := { name }.
		^ false ].
	(RBScanner isVariable: string) ifFalse: [
		violators := { name }.
		^ false ].
	^ true
]
