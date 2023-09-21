Class {
	#name : 'RBRemoveClassReparentChoice',
	#superclass : 'RBRemoveClassChoice',
	#instVars : [
		'classesHaveSubclasses'
	],
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
RBRemoveClassReparentChoice >> action [

	driver removeClassChanges.

]

{ #category : 'as yet unclassified' }
RBRemoveClassReparentChoice >> classesHaveSubclasses: aBoolean [

	classesHaveSubclasses := aBoolean
]

{ #category : 'accessing' }
RBRemoveClassReparentChoice >> description [

	| stream |
	stream := String new writeStream.
	stream nextPutAll: 'Remove it'.

	classesHaveSubclasses ifTrue: [
		stream nextPutAll: ' (reparenting subclasses)' ].

	^ stream contents
]

{ #category : 'accessing' }
RBRemoveClassReparentChoice >> order [
	^1
]
