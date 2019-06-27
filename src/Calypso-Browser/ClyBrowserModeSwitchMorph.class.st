"
I represent the command which switches the browser to particular mode.
Therefore I use radio button widget to reflect that target browser mode is already active.
I ask the activator #isCommandAppliedToBrowser to detect this state.
I implement this logic in the method #isModeActive.

When user click on radio button I just execute the command.

Internal Representation and Key Implementation Points.

    Instance Variables
	radioButton:		<Morph>
"
Class {
	#name : #ClyBrowserModeSwitchMorph,
	#superclass : #ClyBrowserToolbarItemMorph,
	#instVars : [
		'radioButton'
	],
	#category : #'Calypso-Browser-Toolbar'
}

{ #category : #accessing }
ClyBrowserModeSwitchMorph >> addIcon: anIcon [

	"do not supported for now"
]

{ #category : #updating }
ClyBrowserModeSwitchMorph >> beBoldIfActive [

	self isModeActive 
		ifTrue: [ radioButton labelMorph emphasis: TextEmphasis bold emphasisCode]
		ifFalse: [ radioButton labelMorph emphasis: TextEmphasis normal emphasisCode]
]

{ #category : #accessing }
ClyBrowserModeSwitchMorph >> build [
	radioButton := self theme 
		newRadioButtonIn: World 
		for: self 
		getSelected: #isModeActive 
		setSelected: #toggleMode:
		getEnabled: nil
		label: self title
		help: nil.
	"self beBoldIfActive."
	self addMorph: radioButton
]

{ #category : #testing }
ClyBrowserModeSwitchMorph >> isModeActive [
	^activator isCommandAppliedToBrowser
]

{ #category : #controling }
ClyBrowserModeSwitchMorph >> toggleMode: aBool [
	
	self executeCommand
]
