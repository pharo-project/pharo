rendering
addMorphFromObject: anObject toContainer: aContainerMorph
	| renderedMorph |
	renderedMorph := (self renderObject: anObject)
				vResizing: #spaceFill;
				hResizing: #rigid;
				minimumExtent: 200@10;
				extent: 200@10;
				yourself.
	aContainerMorph addMorphBack: renderedMorph.
	aContainerMorph addMorphBack: (EdgeGripMorph new
		width: ProportionalSplitterMorph splitterWidth;
		hResizing: #rigid; "not the default this time, vResizing is also #spaceFill by default"
		target: renderedMorph)