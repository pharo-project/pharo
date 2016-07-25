rendering
newColumnWith: aCollectionOfMorphElements
	"use this method to create a new column in your block"
	<lint: 'Utility methods' rationale: 'defined here to facilate the use of the library' author: 'cyrille.delaunay'>
	
	|answer|
	answer := PanelMorph new
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		fillStyle: Color transparent; "non pane color tracking"
		changeTableLayout;
		cellInset: 8; yourself.
	aCollectionOfMorphElements do: [:m | answer addMorphBack: m].
	^answer