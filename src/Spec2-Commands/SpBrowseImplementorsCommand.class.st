"
Browse all implementors of the selector found in context.
"
Class {
	#name : #SpBrowseImplementorsCommand,
	#superclass : #SpSystemSelectionCommand,
	#category : #'Spec2-Commands-Navigation'
}

{ #category : #default }
SpBrowseImplementorsCommand class >> defaultDescription [
	^ 'Browse all implementors of the selected method or message'
]

{ #category : #default }
SpBrowseImplementorsCommand class >> defaultShortcutKey [

	^ $m command mac 
		| $m control win
		| $m control unix
]

{ #category : #default }
SpBrowseImplementorsCommand class >> shortName [

	^ 'implementors'
]

{ #category : #executing }
SpBrowseImplementorsCommand >> execute [

	self context doBrowseImplementors
]
