Class {
	#name : #SpTextCopyCommand,
	#superclass : #SpTextEditionCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #defaults }
SpTextCopyCommand class >> defaultIconName [
	^#smallCopy
]

{ #category : #default }
SpTextCopyCommand class >> defaultName [

	^ 'Copy'
]

{ #category : #default }
SpTextCopyCommand class >> defaultShortcutKey [

	^ $c command mac 
		| $c control win 
		| $c control unix
]

{ #category : #execution }
SpTextCopyCommand >> execute [
	self editor copySelection
]
