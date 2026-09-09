Class {
	#name : 'MyClassB2',
	#superclass : 'MyClassA',
	#category : 'Refactoring-DataForTesting-StaticModel',
	#package : 'Refactoring-DataForTesting',
	#tag : 'StaticModel'
}

{ #category : 'asserting' }
MyClassB2 >> shouldNotBeOverriddenByAdd [

	^ 666
]
