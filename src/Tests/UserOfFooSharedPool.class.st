"
I am an sunit test for SystemNavigation.
"
Class {
	#name : #UserOfFooSharedPool,
	#superclass : #Object,
	#pools : [
		'FooSharedPool'
	],
	#category : #'Tests-System'
}

{ #category : #'test mock' }
UserOfFooSharedPool >> simpleMethod [
	^FooValue1
]
