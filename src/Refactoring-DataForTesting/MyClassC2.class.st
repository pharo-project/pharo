Class {
	#name : 'MyClassC2',
	#superclass : 'MyClassB2',
	#category : 'Refactoring-DataForTesting-StaticModel',
	#package : 'Refactoring-DataForTesting',
	#tag : 'StaticModel'
}

{ #category : 'accessing - structure variables' }
MyClassC2 >> x [ 
	^super x +10
]
