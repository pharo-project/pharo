accessing
contents
	^ self childrenItems collect: [ :child | self childNodeFromItem: child ]