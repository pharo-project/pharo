visitor
visitCategoryRename: aChange

	^ EpCategoryRename
			oldName: aChange newCategoryName
			newName: aChange oldCategoryName
			packageName: aChange affectedPackageName
