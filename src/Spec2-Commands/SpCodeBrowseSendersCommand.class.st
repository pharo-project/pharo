Class {
	#name : #SpCodeBrowseSendersCommand,
	#superclass : #SpCodeBrowsingCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #defaults }
SpCodeBrowseSendersCommand class >> defaultIconName [
	^#smallSystemBrowser 
]

{ #category : #default }
SpCodeBrowseSendersCommand class >> defaultName [

	^ 'Browse senders'
]

{ #category : #default }
SpCodeBrowseSendersCommand class >> defaultShortcutKey [

	^ $n command mac 
		| $n control win
		| $n control unix
]

{ #category : #executing }
SpCodeBrowseSendersCommand >> execute [
	self browserClientProvider browseAllSendersOf: self selector
]
