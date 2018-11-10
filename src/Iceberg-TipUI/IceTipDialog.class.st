"
I'm a general dialog. 
I implement a default accept method that needs to be overriden my children and some utility methods to easier build a dialog.
"
Class {
	#name : #IceTipDialog,
	#superclass : #IceTipComposablePresenter,
	#instVars : [
		'acceptButton',
		'cancelButton'
	],
	#category : #'Iceberg-TipUI-View'
}

{ #category : #actions }
IceTipDialog >> accept [
	"override this if you want a dialog"
]

{ #category : #accessing }
IceTipDialog >> initializeDialogWindow: aWindow [
	
	acceptButton := aWindow toolbar okButton.
	cancelButton := aWindow toolbar cancelButton.
]

{ #category : #accessing }
IceTipDialog >> openDialogWithSpec [
	^ super openDialogWithSpec
		okAction: [ self accept ];
		yourself
]
