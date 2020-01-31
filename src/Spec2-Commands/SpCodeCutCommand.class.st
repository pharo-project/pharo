Class {
	#name : #SpCodeCutCommand,
	#superclass : #SpCodeEditionCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #defaults }
SpCodeCutCommand class >> defaultIconName [
	^#smallCut 
]

{ #category : #default }
SpCodeCutCommand class >> defaultName [

	^ 'Cut'
]

{ #category : #default }
SpCodeCutCommand class >> defaultShortcutKey [

	^ $x command mac 
		| $x control win 
		| $x control unix
]

{ #category : #execution }
SpCodeCutCommand >> execute [
	self editor cut
]
