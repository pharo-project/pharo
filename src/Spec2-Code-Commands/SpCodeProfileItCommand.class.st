Class {
	#name : #SpCodeProfileItCommand,
	#superclass : #SpCodeSelectionCommand,
	#category : #'Spec2-Code-Commands'
}

{ #category : #defaults }
SpCodeProfileItCommand class >> defaultIconName [
	^#smallProfile 
]

{ #category : #default }
SpCodeProfileItCommand class >> defaultName [

	^ 'Profile it'
]

{ #category : #execution }
SpCodeProfileItCommand >> execute [

	self flag: 'TODO'
]
