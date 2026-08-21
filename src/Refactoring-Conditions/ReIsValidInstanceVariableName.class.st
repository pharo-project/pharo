"
I check if a name is valid for an instance variable
"
Class {
	#name : 'ReIsValidInstanceVariableName',
	#superclass : 'ReVariableNameCondition',
	#instVars : [
		'classSide'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'utilities' }
ReIsValidInstanceVariableName class >> name: aString classSide: aBoolean [

	^ self new
		  name: aString;
		  classSide: aBoolean
]

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

	(OCScanner isVariable: string) ifFalse: [
			violator := name.
			^ false ].

	classSide ifFalse: [
			string first isUppercase ifTrue: [
					violator := name.
					^ false ] ].

	^ true
]

{ #category : 'accessing' }
ReIsValidInstanceVariableName >> classSide: aBoolean [

	classSide := aBoolean
]

{ #category : 'initialization' }
ReIsValidInstanceVariableName >> initialize [

	super initialize.
	classSide := false
]

{ #category : 'accessing' }
ReIsValidInstanceVariableName >> violationMessageOn: aStream [

	violator ifNotNil: [
			^ aStream
				  nextPutAll: violator;
				  nextPutAll: ' the name musn''t start in uppercase or be a reserved word.' ]
]
