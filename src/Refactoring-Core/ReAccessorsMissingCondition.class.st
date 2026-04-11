"
I check if given transformation can find an existing accessor for its variable name
"
Class {
	#name : 'ReAccessorsMissingCondition',
	#superclass : 'ReVariableNameCondition',
	#instVars : [
		'transformation'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
ReAccessorsMissingCondition >> check [

	violator ifNil: [
			violator := OrderedCollection new.
			transformation findGetterMethod ifNotNil: [ :sel | violator add: sel ].
			transformation findSetterMethod ifNotNil: [ :sel | violator add: sel ].
			violator ].
	^ violator size < 2
]

{ #category : 'accessing' }
ReAccessorsMissingCondition >> transformation: aTransformation [
    transformation := aTransformation
]

{ #category : 'displaying' }
ReAccessorsMissingCondition >> violationMessageOn: aStream [

	violator do: [ :sel |
			aStream
				nextPutAll: sel;
				nextPutAll: ' already exists as an accessor for ';
				nextPutAll: transformation variableName;
				nextPutAll: '.';
				space ]
]
