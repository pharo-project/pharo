"
I am a data object representing an action associated to a tab.

I contains
	- an action to be performed when clicked
	- an icon to be rendered
	- a label to add indication about what I am supposed to do
"
Class {
	#name : #TabAction,
	#superclass : #Object,
	#instVars : [
		'action',
		'icon',
		'label'
	],
	#category : 'Morphic-Widgets-Tabs'
}

{ #category : #'instance creation' }
TabAction class >> action: action icon: icon label: label [

	^ self new
		action: action;
		icon: icon;
		label: label;
		yourself
]

{ #category : #accessing }
TabAction >> action [
	^ action
]

{ #category : #accessing }
TabAction >> action: anObject [

	action := anObject.
	icon on: #click send: #value to: action
]

{ #category : #accessing }
TabAction >> icon [
	^ icon
]

{ #category : #accessing }
TabAction >> icon: aForm [

	icon image: aForm.
	label 
		ifNotNil: [ icon setBalloonText: label maxLineLength: 100 ].
	action ifNotNil: [ icon on: #click send: #value to: action ]
]

{ #category : #initialization }
TabAction >> initialize [

	super initialize.

	icon := ImageMorph new
]

{ #category : #accessing }
TabAction >> label [
	^ label
]

{ #category : #accessing }
TabAction >> label: aLabel [

	label := aLabel.
	icon setBalloonText: label maxLineLength: 100
]
