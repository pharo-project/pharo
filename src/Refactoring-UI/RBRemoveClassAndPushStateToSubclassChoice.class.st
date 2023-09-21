Class {
	#name : 'RBRemoveClassAndPushStateToSubclassChoice',
	#superclass : 'RBRemoveClassChoice',
	#instVars : [
		'removingClassesAreEmpty'
	],
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
RBRemoveClassAndPushStateToSubclassChoice >> action [ 

	driver removeClassAndPushStateToSubclasses
]

{ #category : 'accessing' }
RBRemoveClassAndPushStateToSubclassChoice >> description [

	| stream |
	stream := String new writeStream.
	stream nextPutAll: 'Remove it'.

	removingClassesAreEmpty ifFalse: [
		stream nextPutAll: ', pushing class state to subclasses' ].

	^ stream contents
]

{ #category : 'as yet unclassified' }
RBRemoveClassAndPushStateToSubclassChoice >> emptyClasses: aBoolean [

	removingClassesAreEmpty := aBoolean
]

{ #category : 'accessing' }
RBRemoveClassAndPushStateToSubclassChoice >> order [

	^ 2
]
