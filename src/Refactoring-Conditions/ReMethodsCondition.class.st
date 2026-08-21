Class {
	#name : 'ReMethodsCondition',
	#superclass : 'ReReifiedCondition',
	#instVars : [
		'violators'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'checking' }
ReMethodsCondition >> check [ 

	^ self violators isEmpty
]
