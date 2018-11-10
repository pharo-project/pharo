"
I'm an error dialog that is used to show generic errors to users.
This is to enhance the user experience of using iceberg, since exceptions can be sometimes intrusive or aggresive.

My model is an exception that I'll freeze for further debugging.

(IceTipErrorDialog on: anError) openDialogWithSpec
"
Class {
	#name : #IceTipErrorDialog,
	#superclass : #IceTipDialog,
	#instVars : [
		'exception',
		'errorLabel',
		'iconPanel'
	],
	#category : #'Iceberg-TipUI-View'
}

{ #category : #specs }
IceTipErrorDialog class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed 
		newRow: [ :row |
			row 
				newColumn: #iconPanel width: 40 * World displayScaleFactor;
				newColumn: #errorLabel ]
]

{ #category : #specs }
IceTipErrorDialog class >> title [
	^ 'Error'
]

{ #category : #initialization }
IceTipErrorDialog >> accept [
	
	self window delete.
	exception debug.
]

{ #category : #accessing }
IceTipErrorDialog >> errorLabel [
	^ errorLabel
]

{ #category : #accessing }
IceTipErrorDialog >> errorLabel: anObject [
	errorLabel := anObject
]

{ #category : #accessing }
IceTipErrorDialog >> exception [
	^ exception
]

{ #category : #initialization }
IceTipErrorDialog >> exception: anException [

	exception := anException freeze
]

{ #category : #accessing }
IceTipErrorDialog >> iconPanel [
	^ iconPanel
]

{ #category : #accessing }
IceTipErrorDialog >> iconPanel: anObject [
	iconPanel := anObject
]

{ #category : #initialization }
IceTipErrorDialog >> initialExtent [
	^ (500@(
		self class inputTextHeight * 2
		+ self class buttonHeight 
		+ 50 "Yes, a magic number"
		)) scaledByDisplayScaleFactor
]

{ #category : #initialization }
IceTipErrorDialog >> initializeDialogWindow: aWindow [

	super initializeDialogWindow: aWindow.
	acceptButton label: 'Debug'.
	cancelButton label: 'Ignore'.
]

{ #category : #initialization }
IceTipErrorDialog >> initializeWidgets [
	iconPanel := self newIcePanel.
	iconPanel widget addMorph: ((self iconNamed: #error) scaledToSize: 32@32) asMorph.
	
	errorLabel := self newIceReadOnlyText.
	errorLabel widget contentsWrapped: exception messageText.
	errorLabel widget
		hResizing: #spaceFill;
		vResizing: #spaceFill.
]

{ #category : #initialization }
IceTipErrorDialog >> setModelBeforeInitialization: aDomainObject [

	self exception: aDomainObject
]
