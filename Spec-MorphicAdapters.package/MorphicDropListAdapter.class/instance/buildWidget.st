factory
buildWidget

	^ SpecDropListMorph new
		wrapSelector:	 #wrap:withIndex:; 
	    	on: self list: #getList selected: #getIndex changeSelected: #setIndex:;
		hResizing: #spaceFill ;
		vResizing: #spaceFill ;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;	
		setBalloonText: self help;
		yourself