"
I am a vertical line which separates toolbar items belonging to different menu groups. I am created by the toolbar renderer between groups, and by the protocol editor after its checkbox.
"
Class {
	#name : 'ClyToolbarSeparatorMorph',
	#superclass : 'Morph',
	#category : 'Calypso-Browser-Toolbar',
	#package : 'Calypso-Browser',
	#tag : 'Toolbar'
}

{ #category : 'accessing' }
ClyToolbarSeparatorMorph >> defaultColor [
	^Color transparent
]

{ #category : 'initialization' }
ClyToolbarSeparatorMorph >> initialize [
	| separator |
	super initialize.
	self
		changeTableLayout;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		layoutInset: (4@0) scaledByDisplayScaleFactor.
	separator := self theme newLabelIn: self label: '|'.

	self addMorph: separator
]
