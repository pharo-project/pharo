updating
updateChildren
	self childrenDo: [:child | child parent: self]
	
