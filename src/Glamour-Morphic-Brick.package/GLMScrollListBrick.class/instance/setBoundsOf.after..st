layouting
setBoundsOf: aMorph after: afterMorph
	
	aMorph
		withWidth: self band width;
		brickBounds: (afterMorph brickBounds bottomLeft extent: aMorph extent)