Class {
	#name : #PopupChoiceOrRequestDialogWindow,
	#superclass : #PopupChoiceDialogWindow,
	#instVars : [
		'okButton'
	],
	#category : 'Polymorph-Widgets-Windows'
}

{ #category : #'as yet unclassified' }
PopupChoiceOrRequestDialogWindow class >> minWidth [
	^250
]

{ #category : #accessing }
PopupChoiceOrRequestDialogWindow >> extent: aPoint [
	"Make the choices area at least fill the scroll area."
	^super extent: (aPoint x max: self class minWidth)@(aPoint y).
]

{ #category : #accessing }
PopupChoiceOrRequestDialogWindow >> filter: aString [
	"Set the value of filter used to match the choices.
	Clear any selected items here so that potential requests for
	non-listed items can be handled."
	
	super filter: aString.
	(self choiceMenus ifNil: [^self]) do: [:embeddedMenu |
		embeddedMenu selectItem: nil event: nil]. "clear selection in other menus"

]

{ #category : #accessing }
PopupChoiceOrRequestDialogWindow >> filterValue [

	^filter isEmpty
		ifTrue: [ nil ]
		ifFalse: [ filter ]
]

{ #category : #initialization }
PopupChoiceOrRequestDialogWindow >> newButtons [
	"Answer new buttons as appropriate."
	self filterMorph: self newFilterEntry.
	self okButton: self newOKButton.
	^Array 
		with: self filterMorph
		with: self okButton
		with: self newCancelButton
]

{ #category : #accessing }
PopupChoiceOrRequestDialogWindow >> okButton [
	^okButton
]

{ #category : #private }
PopupChoiceOrRequestDialogWindow >> okButton: aMorph [
	okButton := aMorph
]

{ #category : #'event handling' }
PopupChoiceOrRequestDialogWindow >> processEnter: anEvent [
	(super processEnter: anEvent)
		ifFalse: [ self okButton performAction ].
		
	^true
]
