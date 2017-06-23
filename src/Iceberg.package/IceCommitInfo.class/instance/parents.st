accessing
parents
	^ parents ifNil: [ 
		parents := parentIds 
			collect: [ :parentId | self repository commitAt: parentId ] ]