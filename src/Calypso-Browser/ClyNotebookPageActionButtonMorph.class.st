"
I am a multi state button supporting icon.
I am used to render page actions.
"
Class {
	#name : #ClyNotebookPageActionButtonMorph,
	#superclass : #MultistateButtonMorph,
	#instVars : [
		'icon'
	],
	#category : #'Calypso-Browser-Tabs'
}

{ #category : #'instance creation' }
ClyNotebookPageActionButtonMorph class >> forAction: anAction [

	^ self new
		forAction: anAction;
		yourself
]

{ #category : #drawing }
ClyNotebookPageActionButtonMorph >> drawOn: aCanvas [

	super drawOn: aCanvas.
	icon ifNil: [ ^ self ].
	icon form ifNil: [ ^ self ].
	aCanvas
		translucentImage: icon form
		at: self topLeft
]

{ #category : #'instance creation' }
ClyNotebookPageActionButtonMorph >> forAction: anAction [
	| baseColor |
	
	baseColor := self theme baseColor.
	self extent: anAction icon extent.
	
	self activeEnabledNotOverUpFillStyle: baseColor.
	
	self activeDisabledNotOverUpFillStyle: (self theme buttonDisabledFillStyleFor: self).
	self passiveEnabledNotOverUpFillStyle: baseColor.
	self passiveDisabledNotOverUpFillStyle: baseColor.
	
	self
		activeEnabledOverUpFillStyle: (self theme buttonMouseOverFillStyleFor: self);
		passiveEnabledOverUpFillStyle: (self theme buttonMouseOverFillStyleFor: self).
	
	self
		activeEnabledOverDownFillStyle: (self theme buttonPressedFillStyleFor: self);
		passiveEnabledOverDownFillStyle: (self theme buttonPressedFillStyleFor: self);
		addUpAction: [ anAction action value ];
		setBalloonText: anAction label;
		extent: 16@16.
		
	icon := anAction icon.
		
	^ self
]

{ #category : #accessing }
ClyNotebookPageActionButtonMorph >> icon: anObject [
	
	icon := anObject
]

{ #category : #theme }
ClyNotebookPageActionButtonMorph >> themeChanged [
	| baseColor |

	super themeChanged.

	baseColor := self theme baseColor.
	self activeDisabledNotOverUpFillStyle: (self theme buttonDisabledFillStyleFor: self).
	self activeEnabledNotOverUpFillStyle: baseColor.
	self passiveEnabledNotOverUpFillStyle: baseColor.
	self passiveDisabledNotOverUpFillStyle: baseColor.
	self
		activeEnabledOverUpFillStyle: (self theme buttonMouseOverFillStyleFor: self);
		passiveEnabledOverUpFillStyle: (self theme buttonMouseOverFillStyleFor: self);
		activeEnabledOverDownFillStyle: (self theme buttonPressedFillStyleFor: self);
		passiveEnabledOverDownFillStyle: (self theme buttonPressedFillStyleFor: self)
	
]
