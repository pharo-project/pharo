"
I am a flat icon button for the header of the Quick Project Loader: no border and no fill until the mouse is over me.
"
Class {
	#name : 'QuickProjectIconButtonMorph',
	#superclass : 'Morph',
	#instVars : [
		'icon',
		'action',
		'hovering'
	],
	#category : 'QuickProjectLoader-UI',
	#package : 'QuickProjectLoader',
	#tag : 'UI'
}

{ #category : 'instance creation' }
QuickProjectIconButtonMorph class >> icon: aForm help: aString action: aBlock [

	^ self new
		icon: aForm;
		setBalloonText: aString;
		action: aBlock;
		yourself
]

{ #category : 'accessing' }
QuickProjectIconButtonMorph >> action: aBlock [

	action := aBlock
]

{ #category : 'drawing' }
QuickProjectIconButtonMorph >> drawOn: aCanvas [

	hovering ifTrue: [
		aCanvas fillRectangle: self bounds color: self theme lightBackgroundColor ].
	aCanvas
		translucentImage: icon
		at: self center - (icon extent // 2)
]

{ #category : 'event handling' }
QuickProjectIconButtonMorph >> handlesMouseDown: anEvent [

	^ true
]

{ #category : 'event handling' }
QuickProjectIconButtonMorph >> handlesMouseOver: anEvent [

	^ true
]

{ #category : 'accessing' }
QuickProjectIconButtonMorph >> icon: aForm [

	icon := aForm
]

{ #category : 'initialization' }
QuickProjectIconButtonMorph >> initialize [

	super initialize.
	hovering := false.
	self
		color: Color transparent;
		borderWidth: 0;
		useRoundedCorners;
		extent: 26 @ 26
]

{ #category : 'event handling' }
QuickProjectIconButtonMorph >> mouseDown: anEvent [

	anEvent redButtonPressed ifTrue: [ action value ]
]

{ #category : 'event handling' }
QuickProjectIconButtonMorph >> mouseEnter: anEvent [

	hovering := true.
	self changed
]

{ #category : 'event handling' }
QuickProjectIconButtonMorph >> mouseLeave: anEvent [

	hovering := false.
	self changed
]
