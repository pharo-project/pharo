"
I am a data object representing an action associated to a page.
I contains
	- an action to be performed when clicked
	- an icon to be rendered
	- a label to add indication about what I am supposed to do
"
Class {
	#name : #ClyNotebookPageAction,
	#superclass : #Object,
	#instVars : [
		'action',
		'icon',
		'label'
	],
	#category : #'Calypso-Browser-Tabs'
}

{ #category : #'instance creation' }
ClyNotebookPageAction class >> action: action icon: icon label: label [

	^ self new
		action: action;
		icon: icon;
		label: label;
		yourself
]

{ #category : #accessing }
ClyNotebookPageAction >> action [
	^ action
]

{ #category : #accessing }
ClyNotebookPageAction >> action: anObject [

	action := anObject.
	icon on: #click send: #value to: action
]

{ #category : #accessing }
ClyNotebookPageAction >> icon [
	^ icon
]

{ #category : #accessing }
ClyNotebookPageAction >> icon: aForm [

	icon image: aForm.
	label 
		ifNotNil: [ icon setBalloonText: label maxLineLength: 100 ].
	action ifNotNil: [ icon on: #click send: #value to: action ]
]

{ #category : #initialization }
ClyNotebookPageAction >> initialize [

	super initialize.
	icon := ImageMorph new
]

{ #category : #accessing }
ClyNotebookPageAction >> label [
	^ label
]

{ #category : #accessing }
ClyNotebookPageAction >> label: aLabel [

	label := aLabel.
	icon setBalloonText: label maxLineLength: 100
]
