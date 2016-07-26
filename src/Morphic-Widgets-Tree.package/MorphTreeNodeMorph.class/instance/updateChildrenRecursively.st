updating
updateChildrenRecursively
	self childrenDo: [:child | 
		child parent: self. 
		child updateChildrenRecursively]
	
