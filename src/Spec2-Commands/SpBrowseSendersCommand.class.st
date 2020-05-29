"
I browse all senders of the computed context that is transformed to a selector.
"
Class {
	#name : #SpBrowseSendersCommand,
	#superclass : #SpSystemNavigationCommand,
	#category : #'Spec2-Commands-Navigation'
}

{ #category : #default }
SpBrowseSendersCommand class >> defaultDescription [
	^ 'Browse all senders of the selected method or message'
]

{ #category : #default }
SpBrowseSendersCommand class >> defaultShortcutKey [

	^ $n command mac 
		| $n control win
		| $n control unix
]

{ #category : #default }
SpBrowseSendersCommand class >> shortName [

	^ 'senders'
]

{ #category : #executing }
SpBrowseSendersCommand >> execute [

	self systemNavigation browseAllSendersOf: self selector
]
