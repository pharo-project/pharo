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
	name isString ifFalse: [^false].
	string := name asString.
	string isEmpty ifTrue: [^false].
	(self class reservedNames includes: string) ifTrue: [^false].
	string first isUppercase ifTrue: [^false].
	(RBScanner isVariable: string) ifFalse: [^false ].
	^ true
]
