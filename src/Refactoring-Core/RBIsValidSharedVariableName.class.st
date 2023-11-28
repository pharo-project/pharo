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
	name isString ifFalse: [^false].
	string := name asString.
	(self class reservedNames includes: string) ifTrue: [^false].
	string isEmpty ifTrue: [^false].
	string first isUppercase ifFalse: [^false].
	^RBScanner isVariable: string
]
