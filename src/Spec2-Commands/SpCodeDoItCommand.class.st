Class {
	#name : #SpCodeDoItCommand,
	#superclass : #SpCodeSelectionCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #defaults }
SpCodeDoItCommand class >> defaultIconName [
	^#smallDoIt 
]

{ #category : #default }
SpCodeDoItCommand class >> defaultName [

	^ 'Do it'
]

{ #category : #default }
SpCodeDoItCommand class >> defaultShortcutKey [

	^ $d command mac 
		| $d control win 
		| $d control unix
]

{ #category : #execution }
SpCodeDoItCommand >> execute [
	"Treat the current text selection as an expression; evaluate it."
	
	self evaluateSelectionAndDo: [ :result | ]
]
