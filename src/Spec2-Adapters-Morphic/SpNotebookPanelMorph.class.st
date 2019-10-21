Class {
	#name : #SpNotebookPanelMorph,
	#superclass : #PanelMorph,
	#category : #'Spec2-Adapters-Morphic-Notebook'
}

{ #category : #'change reporting' }
SpNotebookPanelMorph >> adoptPaneColor: paneColor [
	"Change our color too."

	self assureExtension.

	extension fillStyle
		ifNil: [ self color: paneColor ].
	self borderStyle baseColor: paneColor darker.
	
	self submorphsDo: [ :m | m adoptPaneColor: paneColor ]
]

{ #category : #'change reporting' }
SpNotebookPanelMorph >> changed [
	"Report that the area occupied by this morph should be redrawn.
	Fixed to include submorphs outside the outerBounds."

	self world
		ifNil: [ ^ self ].

 	^ fullBounds 
		ifNil: [ self invalidRect: self bounds ]
		ifNotNil: [ self invalidRect: fullBounds ]
]

{ #category : #accessing }
SpNotebookPanelMorph >> extent: aPoint [
	| newExtent |	
	newExtent := aPoint rounded.
	(bounds extent closeTo: newExtent)
		ifTrue: [ ^ self ].

	self invalidRect: bounds.
	bounds := bounds topLeft extent: newExtent.
	self layoutChanged.
	self invalidRect: bounds
]
