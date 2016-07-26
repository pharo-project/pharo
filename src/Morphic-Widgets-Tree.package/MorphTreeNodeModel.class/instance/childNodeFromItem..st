accessing
childNodeFromItem: anItem
	^ ((self childNodeClassFromItem: anItem) with: anItem model: model) parentNode: self; yourself