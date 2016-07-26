*NautilusRefactoring
whatToDisplayIn: aBrowser 
	
	^ self changes changes flatCollect: [:e | e whatToDisplayIn: aBrowser ]