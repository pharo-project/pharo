Class {
	#name : 'ReIsValidClassName',
	#superclass : 'ReClassNameCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'initialization' }
ReIsValidClassName >> violators [

	| string |
	className isString ifFalse: [ ^ { className } ].
	string := className asString.
	^ ((Symbol reservedLiterals includes: string) or: [
		 string isEmpty or: [ string first isUppercase not or: [ (OCScanner isVariable: string) not ] ] ])
		ifTrue: [ { className } ]
		ifFalse: [ #(  ) ]
]
