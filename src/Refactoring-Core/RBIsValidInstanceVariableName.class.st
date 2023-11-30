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
		violators := { name }.
		^ false ].
	string := name asString.
	string isEmpty ifTrue: [
		violators := { name }.
		^ false ].
	(self class reservedNames includes: string) ifTrue: [
		violators := { name }.
		^ false ].
	string first isUppercase ifTrue: [
		violators := { name }.
		^ false ].
	(RBScanner isVariable: string) ifFalse: [
		violators := { name }.
		^ false ].
	^ true
]

{ #category : 'accessing' }
RBIsValidInstanceVariableName >> errorString [ 

	^ 'Invalid name {1}. The name mustn''t start in uppercase or be reserved word' format: { name }
]

{ #category : 'accessing' }
RBIsValidInstanceVariableName >> violators [
	^ violators
]
