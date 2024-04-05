Class {
	#name : 'ReRemoveClassReparentChoice',
	#superclass : 'ReRemoveClassChoice',
	#instVars : [
		'classesHaveSubclasses',
		'emptyClasses'
	],
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReRemoveClassReparentChoice >> action [

	driver applyChanges

]

{ #category : 'accessing' }
ReRemoveClassReparentChoice >> classesHaveSubclasses: aBoolean [

	classesHaveSubclasses := aBoolean
]

{ #category : 'accessing' }
ReRemoveClassReparentChoice >> description [

	^ String streamContents: [ :aStream |
		aStream nextPutAll: 'Remove it'.

		classesHaveSubclasses ifTrue: [
			aStream nextPutAll: ' and reparent subclasses' ].
		emptyClasses ifFalse: [ 
			aStream nextPutAll: '. WARNING: you will lose the class state' ] ]
]

{ #category : 'accessing' }
ReRemoveClassReparentChoice >> emptyClasses: aBoolean [

	emptyClasses := aBoolean 
]

{ #category : 'accessing' }
ReRemoveClassReparentChoice >> order [
	^1
]
