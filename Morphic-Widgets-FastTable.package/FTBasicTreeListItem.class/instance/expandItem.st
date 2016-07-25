button-interaction
expandItem

	expanded := true.
	
	children := (dataSource childrenFor: item) collect: [ :each | self treeItemFor: each ].
	
	dataSource updateAvailableRows: self availableRows - 1.