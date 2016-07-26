accessing
allContents
	^ self childrenItems
		collect: [ :d | 
			(self class with: d)
				parentNode: self;
				containerTree: self containerTree;
				yourself ]