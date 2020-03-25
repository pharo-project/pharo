"
I browse all implementors of the computed context that is transformed to a selector.
"
Class {
	#name : #SpBrowseImplementorsCommand,
	#superclass : #SpSystemNavigationCommand,
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
	self systemNavigation browseAllImplementorsOf: self selector
]
