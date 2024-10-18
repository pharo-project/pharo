"
I exist to test refactoring conditions
"
Class {
	#name : 'MyClassAlpha',
	#superclass : 'Object',
	#classVars : [
		'SharedVarA'
	],
	#category : 'Refactoring-DataForTesting-StaticModel',
	#package : 'Refactoring-DataForTesting',
	#tag : 'StaticModel'
}

{ #category : 'dummy methods' }
MyClassAlpha >> fooAccessor [
]

{ #category : 'dummy methods' }
MyClassAlpha >> methodForSupersend [
	^ 42
]

{ #category : 'dummy methods' }
MyClassAlpha >> methodForSupersendInLeaf [

	^ 11
]

{ #category : 'dummy methods' }
MyClassAlpha >> methodOverriden [
	^ 24
]

{ #category : 'dummy methods' }
MyClassAlpha >> methodWithSuperSend [
	"One of my subclass has the same implementation and its sibling not"
	^ super class
]
