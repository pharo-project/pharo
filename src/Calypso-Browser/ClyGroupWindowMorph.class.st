"
I fix bad behaviour of my superclass in context of Calypso requirement.
For example I update group window title to be same as selected tab title.
"
Class {
	#name : #ClyGroupWindowMorph,
	#superclass : #GroupWindowMorph,
	#category : #'Calypso-Browser-UI'
}

{ #category : #updating }
ClyGroupWindowMorph >> activateWindow: aWindow [
	
	| index |
	(self tabGroup pages includes: aWindow) ifTrue: [ 
		index := self tabGroup pages indexOf: aWindow.
		^self tabGroup selectedPageIndex: index].
	
	self addWindow: aWindow.
	aWindow activate; announceOpened
]

{ #category : #updating }
ClyGroupWindowMorph >> activeWindow [
	self tabGroup pages size = 0 ifTrue: [ ^nil].
	self tabGroup selectedPageIndex <= 0 ifTrue: [ ^nil ].
	
	^self tabGroup pages at: self tabGroup selectedPageIndex
]

{ #category : #'dropping/grabbing' }
ClyGroupWindowMorph >> dragTab: aSystemWindow event: anEvent in: aTabLabel [
	"Drag a tab. Remove the window from the organiser and place in hand."
	aSystemWindow announcer unsubscribe: self.
	self tabGroup removePage: aSystemWindow.
	aSystemWindow configureForUnembedding.
	self tabGroup pages size = 0 ifTrue: [ self owner delete ].
	aSystemWindow position: anEvent targetPoint.
	anEvent hand grabMorph: aSystemWindow
]

{ #category : #updating }
ClyGroupWindowMorph >> update: aSymbol [
	"Handle tab changes."

	super update: aSymbol.
	self updateMainWindowTitle 
]

{ #category : #updating }
ClyGroupWindowMorph >> update: aSymbol  with: anObject [
	"Handle tab changes."

	super update: aSymbol with: anObject.
	self updateMainWindowTitle 
]

{ #category : #updating }
ClyGroupWindowMorph >> updateMainWindowTitle [
	
	| activeWindow |
		
	activeWindow := self activeWindow ifNil: [^self].

	self window ifNotNil: [:w | w  setLabel: activeWindow label]
]
