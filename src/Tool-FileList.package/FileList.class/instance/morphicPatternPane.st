user interface
morphicPatternPane
	"Remove the vertical scrollbar since the minHeight would otherwise
	be too large to fit the layout frame. Added here for Pharo since
	FileList2 has been merged into FileList."
	
	^ SearchMorph new
		model: self;
		setIndexSelector: #pattern:;
		getIndexSelector: #pattern;		
		searchList: self class searchList;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		yourself