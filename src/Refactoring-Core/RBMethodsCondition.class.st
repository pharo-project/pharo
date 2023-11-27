Class {
	#name : 'RBMethodsCondition',
	#superclass : 'RBCondition',
	#instVars : [
		'violators'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
RBMethodsCondition >> check [ 

	^ self violators isEmpty
]

{ #category : 'accessing' }
RBMethodsCondition >> violators [

	^ self subclassResponsibility 
]
