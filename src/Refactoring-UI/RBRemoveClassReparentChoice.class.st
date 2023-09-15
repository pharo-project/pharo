Class {
	#name : #RBRemoveClassReparentChoice,
	#superclass : #RBRemoveClassChoice,
	#category : #'Refactoring-UI-Choices'
}

{ #category : #accessing }
RBRemoveClassReparentChoice >> action [

	self halt.
]

{ #category : #accessing }
RBRemoveClassReparentChoice >> description [

	^ 'Remove it (reparenting subclasses)'
]

{ #category : #accessing }
RBRemoveClassReparentChoice >> order [
	^1
]
