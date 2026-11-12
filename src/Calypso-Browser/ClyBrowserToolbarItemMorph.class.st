"
My subclasses are different kind of UI elements which represent particular command in the browser toolbar. 
Instances should be created with command activator: 

	ClyBrowserButton activatingBy: aCommandActivator  

When the browser builds the toolbar, it asks each command declared for the #toolbar interaction to build its toolbar item (ClyCommand>>#toolbarItemFor:).
So command decides what UI element (my subclass) should be used to represent it in the toolbar. 
  
Subclasses should implement method #build which should create actual widged which will perform the command using #executeCommand method.
Also I provide the #title method which is based on menu item name of the command.

Internal Representation and Key Implementation Points.

    Instance Variables
	activator:		<ClyCommandActivator>
"
Class {
	#name : 'ClyBrowserToolbarItemMorph',
	#superclass : 'Morph',
	#instVars : [
		'activator'
	],
	#category : 'Calypso-Browser-Toolbar',
	#package : 'Calypso-Browser',
	#tag : 'Toolbar'
}

{ #category : 'instance creation' }
ClyBrowserToolbarItemMorph class >> activatingBy: aCommandActivator [
	^self new
		activator: aCommandActivator
]

{ #category : 'accessing' }
ClyBrowserToolbarItemMorph >> activator [
	^activator
]

{ #category : 'accessing' }
ClyBrowserToolbarItemMorph >> activator: aCommandActivator [
	activator := aCommandActivator.
	self build
]

{ #category : 'accessing' }
ClyBrowserToolbarItemMorph >> addIcon: anIcon [

	self addMorphFront: anIcon asMorph
]

{ #category : 'initialization' }
ClyBrowserToolbarItemMorph >> build [
]

{ #category : 'accessing' }
ClyBrowserToolbarItemMorph >> defaultColor [
	^Color transparent
]

{ #category : 'operations' }
ClyBrowserToolbarItemMorph >> executeCommand [
	activator executeCommand
]

{ #category : 'initialization' }
ClyBrowserToolbarItemMorph >> initialize [
	super initialize.
	self
		changeTableLayout;
		listDirection: #leftToRight;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		cellInset: 4 scaledByDisplayScaleFactor
]

{ #category : 'printing' }
ClyBrowserToolbarItemMorph >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	activator printOn: aStream.
	aStream nextPut: $)
]

{ #category : 'accessing' }
ClyBrowserToolbarItemMorph >> title [
	^activator menuItemName
]
