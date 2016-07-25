display
itemMorphFor: anObject index: anIndex
	
	^ (super itemMorphFor: anObject index: anIndex)
		hResizing: #spaceFill;
		yourself