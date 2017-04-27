visitor
visitCategoryAddition: aChange

	^ EpCategoryRemoval 
			named: aChange categoryName
			packageName: aChange affectedPackageName