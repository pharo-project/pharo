Class {
	#name : 'RBRemoveClassReparentChoice',
	#superclass : 'RBRemoveClassChoice',
	#instVars : [
		'classesHaveSubclasses',
		'emptyClasses'
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
		stream nextPutAll: ' and reparent subclasses' ].
	emptyClasses ifFalse: [ 
		stream nextPutAll: '. Pay attention you lose all the class state.' ].

	^ stream contents
]

{ #category : 'accessing' }
RBRemoveClassReparentChoice >> emptyClasses: aBoolean [

	emptyClasses := aBoolean 
]

{ #category : 'accessing' }
RBRemoveClassReparentChoice >> order [
	^1
]
