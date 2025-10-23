Class {
	#name : 'ReMethodsCondition',
	#superclass : 'ReReifiedCondition',
	#instVars : [
		'violators',
		'selectors',
		'class'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'accessing - private' }
ReMethodsCondition >> candidates [
	"My candidates are method selectors, i.e. symbols"
	^ selectors
]

{ #category : 'checking' }
ReMethodsCondition >> check [ 

	^ self violators isEmpty
]
