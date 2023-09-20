Class {
	#name : 'RBRemoveClassReparentChoice',
	#superclass : 'RBRemoveClassChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
RBRemoveClassReparentChoice >> action [

	driver removeClassChanges.

]

{ #category : 'accessing' }
RBRemoveClassReparentChoice >> description [

	^ 'Remove it (reparenting subclasses)'
]

{ #category : 'accessing' }
RBRemoveClassReparentChoice >> order [
	^1
]
