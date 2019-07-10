"
A button with an icon and label associated with an action and a badge 
"
Class {
	#name : #ToolBarButton,
	#superclass : #ToolBarItemPresenter,
	#instVars : [
		'label',
		'icon',
		'action',
		'badge',
		'help'
	],
	#category : #'Spec-Core-Widgets-Toolbar'
}

{ #category : #specs }
ToolBarButton class >> adapterName [

	^ #ToolBarButtonAdapter
]

{ #category : #accessing }
ToolBarButton >> action [
	^ action
]

{ #category : #accessing }
ToolBarButton >> action: anObject [
	action := anObject
]

{ #category : #accessing }
ToolBarButton >> badge [
	^ badge
]

{ #category : #accessing }
ToolBarButton >> badge: aString [
	badge := aString
]

{ #category : #factory }
ToolBarButton >> buildMorphOnToolbar: aToolbar [
	| button |
	
	button := (ToolBarButtonMorph
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

{ #category : #simulating }
ToolBarButton >> click [
	
	self execute
]

{ #category : #execution }
ToolBarButton >> execute [
	self action cull: self
]

{ #category : #accessing }
ToolBarButton >> help [
	^ help
]

{ #category : #accessing }
ToolBarButton >> help: anObject [
	help := anObject
]

{ #category : #accessing }
ToolBarButton >> icon [
	^ icon
]

{ #category : #accessing }
ToolBarButton >> icon: anObject [
	icon := anObject
]

{ #category : #accessing }
ToolBarButton >> label [
	^ label
]

{ #category : #accessing }
ToolBarButton >> label: anObject [
	label := anObject
]
