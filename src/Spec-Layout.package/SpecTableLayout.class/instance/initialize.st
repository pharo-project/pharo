initialization
initialize 
	super initialize.
	
	commands := OrderedCollection new.
	layoutProperties := SpecTableLayoutProperties new.
	layoutProperties 
		reverseTableCells: true;
		hResizing: #spaceFill;
		vResizing: #spaceFill.