Class {
	#name : 'RBRemoveClassAndPushStateToSubclassChoice',
	#superclass : 'RBRemoveClassChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
RBRemoveClassAndPushStateToSubclassChoice >> action [ 

	self halt.
]

{ #category : 'accessing' }
RBRemoveClassAndPushStateToSubclassChoice >> description [

	^ 'Remove it, pushing class state to subclasses'
]

{ #category : 'accessing' }
RBRemoveClassAndPushStateToSubclassChoice >> order [

	^ 2
]
