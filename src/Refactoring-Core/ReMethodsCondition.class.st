Class {
	#name : 'ReMethodsCondition',
	#superclass : 'RBCondition',
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

{ #category : 'accessing' }
ReMethodsCondition >> violators [

	^ self subclassResponsibility 
]
