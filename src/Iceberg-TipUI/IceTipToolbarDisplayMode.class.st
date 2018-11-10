"
I'm a toolbar display mode.
Toolbars can be displayed in different ways: icons and labels (the default), just icons and just labels (other configurations could be added).

My children implement this display modes.

aToolbar displayModel: IceTipToolbarDisplayMode modeIconAndLabel
"
Class {
	#name : #IceTipToolbarDisplayMode,
	#superclass : #Object,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Iceberg-TipUI-Spec-ItemBar'
}

{ #category : #accessing }
IceTipToolbarDisplayMode class >> default [
	^ self modeIconAndLabel
]

{ #category : #accessing }
IceTipToolbarDisplayMode class >> modeIcon [
	^ IceTipToolbarDisplayModeIcon uniqueInstance
]

{ #category : #accessing }
IceTipToolbarDisplayMode class >> modeIconAndLabel [
	^ IceTipToolbarDisplayModeIconAndLabel uniqueInstance
]

{ #category : #accessing }
IceTipToolbarDisplayMode class >> modeLabel [
	^ IceTipToolbarDisplayModeLabel uniqueInstance
]

{ #category : #'instance creation' }
IceTipToolbarDisplayMode class >> new [
	self error: 'Use #uniqueInstance'
]

{ #category : #'instance creation' }
IceTipToolbarDisplayMode class >> uniqueInstance [
	self = IceTipToolbarDisplayMode 
		ifTrue: [ self error: 'I''m abstract. Use one of my children' ].
	^ uniqueInstance ifNil: [ uniqueInstance := self basicNew initialize ]
]

{ #category : #configuring }
IceTipToolbarDisplayMode >> configureButton: aButton item: aToolbarItem [
	self subclassResponsibility
]

{ #category : #accessing }
IceTipToolbarDisplayMode >> extent [
	^ self subclassResponsibility
]

{ #category : #accessing }
IceTipToolbarDisplayMode >> height [
	^ self extent y
]
