Class {
	#name : #SpCodeBrowseImplementorsCommand,
	#superclass : #SpCodeBrowsingCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #defaults }
SpCodeBrowseImplementorsCommand class >> defaultIconName [
	^#smallSystemBrowser 
]

{ #category : #default }
SpCodeBrowseImplementorsCommand class >> defaultName [

	^ 'Browse implementors'
]

{ #category : #default }
SpCodeBrowseImplementorsCommand class >> defaultShortcutKey [

	^ $m command mac 
		| $m control win
		| $m control unix
]

{ #category : #executing }
SpCodeBrowseImplementorsCommand >> execute [
	self browserClientProvider browseAllImplementorsOf: self selector
]
