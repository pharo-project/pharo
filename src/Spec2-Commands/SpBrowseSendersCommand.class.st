"
Browse all senders.

"
Class {
	#name : #SpBrowseSendersCommand,
	#superclass : #SpSystemSelectionCommand,
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
	| env variableOrClassName |

	variableOrClassName := self context selectedTextOrLine trimmed asSymbol.
	variableOrClassName ifEmpty: [ ^ self ].
	 
	env := self context environment.
	env isBehavior ifTrue: [
		(env hasSlotNamed: variableOrClassName) ifTrue: [
		 ^ self systemNavigation browseAllAccessesTo: variableOrClassName from: env ] ].

	(env bindingOf: variableOrClassName) 
		ifNotNil: [ :ref | ^ self systemNavigation browseAllSendersOf: ref ].
	
	"If this is not a class, behave as SpBrowseMethodReferencesCommand"
	self flag: #TODO. "This thingy waits for Guille's refactor"
	self systemNavigation browseAllReferencesTo: self selectedSelector
		
		
]
