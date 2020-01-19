Class {
	#name : #SpCodeProfileItCommand,
	#superclass : #SpCodeSelectionCommand,
	#category : #'Spec2-Commands-Code'
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

	'OK' crLog
]
