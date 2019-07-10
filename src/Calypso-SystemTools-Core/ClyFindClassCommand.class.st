"
I am a command to find class and show it in the browser.

By default I am executed by cmd+f shortcut
"
Class {
	#name : #ClyFindClassCommand,
	#superclass : #ClyBrowserCommand,
	#category : #'Calypso-SystemTools-Core-Commands-Classes'
}

{ #category : #accessing }
ClyFindClassCommand >> defaultMenuIconName [
	^#smallFindIcon
]

{ #category : #accessing }
ClyFindClassCommand >> defaultMenuItemName [
	^'Find class'
]

{ #category : #execution }
ClyFindClassCommand >> execute [

	| class |
	class := browser searchDialog 
		requestSingleObject: 'Choose class' from: ClyAllClassesQuery sorted.
	
	browser selectClass: class
]
