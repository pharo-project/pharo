Class {
	#name : #SpCodeInspectItCommand,
	#superclass : #SpCodeSelectionCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #defaults }
SpCodeInspectItCommand class >> defaultIconName [
	^#smallInspectIt
]

{ #category : #default }
SpCodeInspectItCommand class >> defaultName [

	^ 'Inspect it'
]

{ #category : #default }
SpCodeInspectItCommand class >> defaultShortcutKey [

	^ $i command mac 
		| $i control win
		| $i control unix
]

{ #category : #execution }
SpCodeInspectItCommand >> execute [
	"Treat the current text selection as an expression; evaluate it."
	
	self evaluateSelectionAndDo: [ :result | 
		self inspectObject: result ]
]

{ #category : #execution }
SpCodeInspectItCommand >> inspectObject: anObject [
	anObject inspect
]
