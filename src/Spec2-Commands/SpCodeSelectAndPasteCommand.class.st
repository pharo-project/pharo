Class {
	#name : #SpCodeSelectAndPasteCommand,
	#superclass : #SpCodeEditionCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #defaults }
SpCodeSelectAndPasteCommand class >> defaultIconName [
	^#smallCopy 
]

{ #category : #default }
SpCodeSelectAndPasteCommand class >> defaultName [

	^ 'Paste...'
]

{ #category : #execution }
SpCodeSelectAndPasteCommand >> execute [
	self editor pasteRecent
]
