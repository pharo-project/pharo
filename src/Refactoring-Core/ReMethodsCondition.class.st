Class {
	#name : 'ReMethodsCondition',
	#superclass : 'ReReifiedCondition',
	#instVars : [
		'violators'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
ReMethodsCondition >> check [ 

	^ self violators isEmpty
]
