Class {
	#name : 'ReRemoveClassAndPushStateToSubclassChoice',
	#superclass : 'ReRemoveClassChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReRemoveClassAndPushStateToSubclassChoice >> action [ 

	driver removeClassAndPushStateToSubclasses
]

{ #category : 'accessing' }
ReRemoveClassAndPushStateToSubclassChoice >> description [

	^ 'Remove it and push class state to subclasses'
]

{ #category : 'accessing' }
ReRemoveClassAndPushStateToSubclassChoice >> order [

	^ 2
]
