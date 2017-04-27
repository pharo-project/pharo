opening
buildMorph

	| morph ft |
	
	morph := AlignmentMorph newColumn
		color: Color transparent;  
		cellInset: 4@4;
		yourself.
	
	textModel := textModel newTextField
		textFont: self aquireFont;
		hResizing: #spaceFill;
		yourself.
		
	ft := FTTableMorph new
		dataSource: dataSource;
		vResizing: #spaceFill; 
		hResizing: #spaceFill;
		yourself.

	morph 
		addMorph: ft;
		addMorph: stringMorph;
		addMorph: textModel.
	
	^ morph
	