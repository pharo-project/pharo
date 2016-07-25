initialization
initialize 
	super initialize.
	
	commands := OrderedCollection new.
	layoutProperties := TableLayoutProperties new.
	layoutProperties 
		reverseTableCells: true;
		hResizing: #spaceFill;
		vResizing: #spaceFill.