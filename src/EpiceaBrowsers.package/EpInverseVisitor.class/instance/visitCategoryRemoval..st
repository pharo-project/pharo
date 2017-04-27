visitor
visitCategoryRemoval: aChange
	
	^ EpCategoryAddition
			named: aChange categoryName
			packageName: aChange affectedPackageName