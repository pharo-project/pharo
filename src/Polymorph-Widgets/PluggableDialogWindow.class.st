"
Pluggable form of dialog window supporting custom selector on model for applying changes along with configurable content and buttons.
"
Class {
	#name : #PluggableDialogWindow,
	#superclass : #ModelDependentDialogWindow,
	#instVars : [
		'statusValue',
		'buttons',
		'contentMorph',
		'applyChangesSelector',
		'defaultFocusMorph'
	],
	#category : #Polymorph-Widgets-Windows
}

{ #category : #'as yet unclassified' }
PluggableDialogWindow >> applyChanges [
	"Apply the changes."

	super applyChanges.
	self applyChangesSelector ifNotNil: [:s |
		self model perform: s withEnoughArguments: {self}]
]

{ #category : #accessing }
PluggableDialogWindow >> applyChangesSelector [
	"Answer the value of applyChangesSelector"

	^ applyChangesSelector
]

{ #category : #accessing }
PluggableDialogWindow >> applyChangesSelector: anObject [
	"Set the value of applyChangesSelector"

	applyChangesSelector := anObject
]

{ #category : #accessing }
PluggableDialogWindow >> buttons [
	"Answer the value of buttons"

	^ buttons
]

{ #category : #accessing }
PluggableDialogWindow >> buttons: anObject [
	"Set the value of buttons"

	buttons := anObject
]

{ #category : #accessing }
PluggableDialogWindow >> contentMorph [
	"Answer the value of contentMorph"

	^ contentMorph
]

{ #category : #accessing }
PluggableDialogWindow >> contentMorph: anObject [
	"Set the value of contentMorph"

	contentMorph := anObject
]

{ #category : #'as yet unclassified' }
PluggableDialogWindow >> defaultFocusMorph [
	^ defaultFocusMorph ifNil: [super defaultFocusMorph]
]

{ #category : #'as yet unclassified' }
PluggableDialogWindow >> defaultFocusMorph: aMorph [
	defaultFocusMorph := aMorph
]

{ #category : #initialization }
PluggableDialogWindow >> initialize [
	"Initialize the receiver."

	super initialize.
	self
		buttons: super newButtons
]

{ #category : #'as yet unclassified' }
PluggableDialogWindow >> newButtons [
	"Answer  the plugged buttons."

	^self buttons
]

{ #category : #'as yet unclassified' }
PluggableDialogWindow >> newContentMorph [
	"Answer the plugged content."
	
	^self contentMorph  
]

{ #category : #'as yet unclassified' }
PluggableDialogWindow >> statusValue [
	^statusValue
]

{ #category : #'as yet unclassified' }
PluggableDialogWindow >> statusValue: val [
	statusValue := val
]

{ #category : #'as yet unclassified' }
PluggableDialogWindow >> useDefaultOKButton [
	"Set the buttons to be just an OK button.
	Only effective before the model is set."
	
	self buttons: {self newOKButton isDefault: true}
]

{ #category : #'as yet unclassified' }
PluggableDialogWindow >> useDefaultOKCancelButton [
	"Set the buttons to be a default OK button and a cancel button.
	Only effective before the model is set."
	
	self buttons: super newButtons
]

{ #category : #'as yet unclassified' }
PluggableDialogWindow >> useOKDefaultCancelButton [
	"Set the buttons to be an OK button and a default cancel button.
	Only effective before the model is set."
	
	self buttons: {self newOKButton. self newCancelButton isDefault: true}
]
