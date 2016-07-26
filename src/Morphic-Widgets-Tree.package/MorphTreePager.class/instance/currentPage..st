accessing
currentPage: anIndex
	anIndex = self currentPage ifTrue: [^ self].
	anIndex = 1 
		ifTrue: [self setPageInterval: (1 to: (pageSize min: self nodeList size))]
		ifFalse: [ | lastIdx  |
			lastIdx := (anIndex * pageSize).
			lastIdx <= self nodeList size 
				ifTrue: [self setPageInterval: (((lastIdx - pageSize + 1) max: 1) to: (lastIdx))]
				ifFalse: [self setPageInterval: ((((self lastPage - 1) * pageSize) + 1) to: self nodeList size)]]
