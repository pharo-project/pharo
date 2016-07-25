initialization
initializeText: aTxModel 
	cursorMorph := TxCaretCursor new.
	self addMorph: cursorMorph.

	layoutViewMorph := self newViewMorphOn: aTxModel.
	layoutViewMorph 
"		hResizing: #spaceFill;
		vResizing: #spaceFill".
	self addMorph: layoutViewMorph.

 	
	self initializeController.
	self color: Color white