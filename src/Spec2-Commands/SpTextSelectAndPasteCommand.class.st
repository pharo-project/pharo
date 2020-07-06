Class {
	#name : #SpTextSelectAndPasteCommand,
	#superclass : #SpTextEditionCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #defaults }
SpTextSelectAndPasteCommand class >> defaultIconName [
	^#smallCopy 
]

{ #category : #default }
SpTextSelectAndPasteCommand class >> defaultName [

	^ 'Paste...'
]

{ #category : #execution }
SpTextSelectAndPasteCommand >> execute [
	self editor pasteRecent
]
