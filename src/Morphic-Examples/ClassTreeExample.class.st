"
ClassTreeExample new openOn: Object

"
Class {
	#name : #ClassTreeExample,
	#superclass : #MorphTreeModel,
	#instVars : [
		'rootClass'
	],
	#category : 'Morphic-Examples-MorphTreeWidget'
}

{ #category : #examples }
ClassTreeExample class >> example [

	ClassTreeExample new openOn: Object

]

{ #category : #accessing }
ClassTreeExample >> keyStroke: anEvent from: aTreeMorph [ 
	self selectedNode
		ifNotNil: [:current | current keyStroke: anEvent from: aTreeMorph]
]

{ #category : #accessing }
ClassTreeExample >> menu: menu shifted: b [ 
	"Set up the menu to apply to the receiver's, 
	honoring the #shifted boolean"
	super menu: menu shifted: b.
	menu addLine.
	self selectedNode ifNotNil: [:current | current menu: menu shifted: b.
		menu addLine.
		menu
			add: 'Expand all from here'
			target: self
			selector: #expandAllFromNode:
			argument: current].
	^ menu
]

{ #category : #'instance creation' }
ClassTreeExample >> open [
	^ self openOn: Object

]

{ #category : #'instance creation' }
ClassTreeExample >> openOn: aClass [
	| window |
	self rootClass: aClass.
	window := StandardWindow new model: self.
	window title: aClass name, ' hierarchy'.
	window 
		addMorph: self treeMorph
		fullFrame: LayoutFrame identity.						
	window themeChanged.
	window openInWorld.
	^ window
]

{ #category : #accessing }
ClassTreeExample >> rootClass [
	^ rootClass ifNil: [rootClass := Object]
]

{ #category : #accessing }
ClassTreeExample >> rootClass: aClass [
	 rootClass := aClass
]

{ #category : #accessing }
ClassTreeExample >> rootItems [
	^ OrderedCollection with: self rootClass
]

{ #category : #accessing }
ClassTreeExample >> rootNodeClassFromItem: anItem [
	^ ClassTreeNodeExample
]

{ #category : #'user interface' }
ClassTreeExample >> treeMorph [
	| treeMorph |
	treeMorph := (MorphTreeMorph on: self)
				beMultiple;
				columns: {
					MorphTreeColumn new startWidth: 300; rowMorphGetSelector: #classButton; headerButtonLabel: 'Class' font:  nil.
					MorphTreeColumn new startWidth: 500; rowMorphGetSelector: #commentText; headerButtonLabel: 'Comment' font:  nil
				};
				rowInset: 4;
				treeLineWidth: 1;
				columnInset: 4;
				getMenuSelector: #menu:shifted:;
				rowColorForEven: Color lightGray muchLighter.
	^ treeMorph buildContents
]
