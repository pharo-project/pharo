"
I'm a toolbar item. 
I define elements in a toolbar.

Items can have label, icon, help text, a badge and or course, an action.
They can also have a position (an instance of IceTipToolbarItemPosition): they can be at left or right of the toolbar.
"
Class {
	#name : #IceTipToolbarItem,
	#superclass : #Object,
	#instVars : [
		'label',
		'icon',
		'action',
		'help',
		'position',
		'badge'
	],
	#category : #'Iceberg-TipUI-Spec-ItemBar'
}

{ #category : #accessing }
IceTipToolbarItem >> action [
	^ action
]

{ #category : #accessing }
IceTipToolbarItem >> action: anObject [
	action := anObject
]

{ #category : #accessing }
IceTipToolbarItem >> badge [
	^ badge
]

{ #category : #accessing }
IceTipToolbarItem >> badge: aString [
	badge := aString
]

{ #category : #accessing }
IceTipToolbarItem >> bePositionLeft [
	self position: IceTipItemPosition left
]

{ #category : #accessing }
IceTipToolbarItem >> bePositionRight [
	self position: IceTipItemPosition right
]

{ #category : #execution }
IceTipToolbarItem >> execute [
	self action cull: self
]

{ #category : #accessing }
IceTipToolbarItem >> help [
	^ help
]

{ #category : #accessing }
IceTipToolbarItem >> help: anObject [
	help := anObject
]

{ #category : #accessing }
IceTipToolbarItem >> icon [
	^ icon
]

{ #category : #accessing }
IceTipToolbarItem >> icon: anObject [
	icon := anObject
]

{ #category : #accessing }
IceTipToolbarItem >> label [
	^ label
]

{ #category : #accessing }
IceTipToolbarItem >> label: anObject [
	label := anObject
]

{ #category : #factory }
IceTipToolbarItem >> newToolbarButtonOn: aToolbar [
	| button |
	
	button := (IceTipToolbarItemButtonMorph
		on: self 
			getState: nil 
			action:  #execute)
		helpText: self help;
		font: aToolbar toolbarItemFont;
		beIconTop;
		hResizing: #rigid;
		vResizing: #spaceFill;
		borderWidth: 0;
		borderColor: Color transparent;		
		cellPositioning: #center;
		width: aToolbar toolbarItemSize;
		yourself.
		
	aToolbar displayMode 
		configureButton: button 
		item: self.

	^ button
]

{ #category : #accessing }
IceTipToolbarItem >> position [
	^ position ifNil: [ IceTipItemPosition default ]
]

{ #category : #accessing }
IceTipToolbarItem >> position: aPosition [
	position := aPosition
]
