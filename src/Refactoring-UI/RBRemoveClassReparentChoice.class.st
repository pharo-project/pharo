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

	driver applyChanges

]

{ #category : 'accessing' }
RBRemoveClassReparentChoice >> classesHaveSubclasses: aBoolean [

	classesHaveSubclasses := aBoolean
]

{ #category : 'accessing' }
RBRemoveClassReparentChoice >> description [

	^ String streamContents: [ :aStream |
		aStream nextPutAll: 'Remove it'.

		classesHaveSubclasses ifTrue: [
			aStream nextPutAll: ' and reparent subclasses' ].
		emptyClasses ifFalse: [ 
			aStream nextPutAll: '. Pay attention you lose all the class state' ] ]
]

{ #category : 'accessing' }
RBRemoveClassReparentChoice >> emptyClasses: aBoolean [

	emptyClasses := aBoolean 
]

{ #category : 'accessing' }
RBRemoveClassReparentChoice >> order [
	^1
]
