rendering
addMorphFromObject: anObject toContainer: aContainerMorph

	| renderedMorph |
	renderedMorph := (self renderObject: anObject).
				"vResizing: #spaceFill;
				hResizing: #rigid;
				minimumExtent: 200@10;
				extent: 200@10;
				yourself."
	aContainerMorph addMorphBack: renderedMorph.