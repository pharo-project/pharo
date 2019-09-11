"
I am the default layout for variable sized objects such as Array.

I contain a fixed number of Slots plus.
Instances of classes using this kind of layout have only a minimum given size. Instances have a custom number of additional fields which can be accessed with an index.
"
Class {
	#name : #VariableLayout,
	#superclass : #PointerLayout,
	#category : #'Slot-Core-Layout'
}

{ #category : #'instance creation' }
VariableLayout class >> extending: superLayout scope: aScope host: aClass [
	^ (superLayout extendVariable: aScope)
		host: aClass;
		yourself
]

{ #category : #format }
VariableLayout >> instanceSpecification [
	^ self hasFields
		ifTrue: [ 3 ]
		ifFalse: [ 2 ]
]

{ #category : #testing }
VariableLayout >> isVariable [
	^ true
]
