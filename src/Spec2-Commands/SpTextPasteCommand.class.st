Class {
	#name : #SpTextPasteCommand,
	#superclass : #SpTextEditionCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #defaults }
SpTextPasteCommand class >> defaultIconName [
	^#smallPaste
]

{ #category : #default }
SpTextPasteCommand class >> defaultName [

	^ 'Paste'
]

{ #category : #default }
SpTextPasteCommand class >> defaultShortcutKey [

	^ $v command mac 
		| $v control win 
		| $v control unix
]

{ #category : #execution }
SpTextPasteCommand >> execute [
	self editor paste
]
