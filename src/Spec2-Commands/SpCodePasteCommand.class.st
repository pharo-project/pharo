Class {
	#name : #SpCodePasteCommand,
	#superclass : #SpCodeEditionCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #defaults }
SpCodePasteCommand class >> defaultIconName [
	^#smallPaste
]

{ #category : #default }
SpCodePasteCommand class >> defaultName [

	^ 'Paste'
]

{ #category : #default }
SpCodePasteCommand class >> defaultShortcutKey [

	^ $v command mac 
		| $v control win 
		| $v control unix
]

{ #category : #execution }
SpCodePasteCommand >> execute [
	self editor paste
]
