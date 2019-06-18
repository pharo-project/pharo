"
I represent browser tab status bar.
I provide two panels where items can be added: 

- commandBar
- contextBar 

It should be redone because it is completely strange names and separation of these panels.

Internal Representation and Key Implementation Points.

    Instance Variables
	commandBar:		<Morph>
	contextBar:		<Morph>

"
Class {
	#name : #ClyStatusBarMorph,
	#superclass : #PanelMorph,
	#instVars : [
		'contextBar',
		'commandBar'
	],
	#category : #'Calypso-Browser-UI'
}

{ #category : #accessing }
ClyStatusBarMorph >> addCommandItem: aMorph [
	commandBar addMorphBack: aMorph.
	^aMorph
]

{ #category : #accessing }
ClyStatusBarMorph >> addContextItem: aMorph [
	contextBar addMorphBack: aMorph.
	^aMorph
]

{ #category : #initialization }
ClyStatusBarMorph >> buildCommandBar [
	commandBar := Morph new
		setNameTo: 'command bar';
		changeTableLayout;
		listDirection: #rightToLeft;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		cellInset: 4@0;
		color: Color transparent;
		height: 0;
		minHeight: 0.
	self addMorphBack: commandBar
]

{ #category : #initialization }
ClyStatusBarMorph >> buildContextBar [
	contextBar := Morph new
		setNameTo: 'context bar';
		changeTableLayout;
		listDirection: #leftToRight;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		cellInset: 4@0;
		color: Color transparent;
		height: 0;
		minHeight: 0.
	self addMorphBack: contextBar
]

{ #category : #accessing }
ClyStatusBarMorph >> clear [
	commandBar removeAllMorphs.
	contextBar removeAllMorphs 
]

{ #category : #initialization }
ClyStatusBarMorph >> initialize [
	super initialize.	
	self 
		color: self theme windowColor;
		changeTableLayout;
		listDirection: #leftToRight;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		height: 0;
		minHeight: 0;
		layoutInset: 10@2.
	self buildContextBar.
	self buildCommandBar
]

{ #category : #updating }
ClyStatusBarMorph >> update [
	"Here I just trying to reset extent.
	When there are no items it should hide it.
	Otherwise layout policy will ignore it".
	(contextBar submorphs isEmpty and: [commandBar submorphs isEmpty]) 
		ifTrue: [ 
			self height: 0.
			contextBar height: 0.
			commandBar height: 0]
]
