Class {
	#name : #SpCodeCopyCommand,
	#superclass : #SpCodeEditionCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #defaults }
SpCodeCopyCommand class >> defaultIconName [
	^#smallCopy
]

{ #category : #default }
SpCodeCopyCommand class >> defaultName [

	^ 'Copy'
]

{ #category : #default }
SpCodeCopyCommand class >> defaultShortcutKey [

	^ $c command mac 
		| $c control win 
		| $c control unix
]

{ #category : #execution }
SpCodeCopyCommand >> execute [
	self editor copySelection
]
