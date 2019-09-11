"
I am a vertical line which is used to separate toolbar elements which belongs to different menu groups.

I am used by CmdMenuGroup to build toolbar items: 

	menuGroup buildBrowserToolbar: aToolbar
"
Class {
	#name : #ClyToolbarSeparatorMorph,
	#superclass : #Morph,
	#category : #'Calypso-Browser-Toolbar'
}

{ #category : #accessing }
ClyToolbarSeparatorMorph >> defaultColor [
	^Color transparent
]

{ #category : #initialization }
ClyToolbarSeparatorMorph >> initialize [
	| separator |
	super initialize.
	self 
		changeTableLayout;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		layoutInset: 4@0.
	separator := self theme newLabelIn: self label: '|'.
		
	self addMorph: separator
]
