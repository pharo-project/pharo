"
I implement toobar in the browser (middle panel).
I collect all commands annotated by ClyToolbarCommandActivation strategy and ask them to build toolbar items.

This logic is implemented in method #updateItems. You can ask it anytime to update toolbar in the browser.
It is based on Commander menu:

	menu := CmdMenu activatedBy: ClyToolbarCommandActivation.

But in contrast to context menu all toolbar groups inline UI items into single toolbar panel.
So the menu groups in toolbar only allow to group several commands together to be close to each other. 

My instances are created on the browser: 

	ClyToolbar of: aBrowser
	 
Internal Representation and Key Implementation Points.

    Instance Variables
	browser:		<ClyBrowser>
"
Class {
	#name : #ClyToolbarMorph,
	#superclass : #PanelMorph,
	#instVars : [
		'browser'
	],
	#category : #'Calypso-Browser-Toolbar'
}

{ #category : #'instance creation' }
ClyToolbarMorph class >> of: aBrowser [
	^self new 
		browser: aBrowser
]

{ #category : #accessing }
ClyToolbarMorph >> addNewItem: aMorph [
	self addMorphBack: aMorph
]

{ #category : #accessing }
ClyToolbarMorph >> browser [
	^ browser
]

{ #category : #accessing }
ClyToolbarMorph >> browser: anObject [
	browser := anObject
]

{ #category : #initialization }
ClyToolbarMorph >> defaultHeight [
	^30
]

{ #category : #initialization }
ClyToolbarMorph >> initialize [
	super initialize.
	self 
		color: self theme windowColor;
		changeTableLayout;
		height: self defaultHeight;	
		layoutInset: 2@0; 
		listDirection: #leftToRight;
		wrapCentering: #center;
		hResizing: #spaceFill;
		vResizing: #rigid
]

{ #category : #updating }
ClyToolbarMorph >> updateItems [

	| menu |
	menu := CmdMenu activatedBy: ClyToolbarActivation.
	browser navigationContextsDo: [ :eachContext | menu buildInContext: eachContext].
	
	self hide.
	self removeAllMorphs.
	[menu buildBrowserToolbar: self] ensure: [self show]
]
