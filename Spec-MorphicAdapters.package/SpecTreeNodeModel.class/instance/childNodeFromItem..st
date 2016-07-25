building
childNodeFromItem: anItem

	^ (anItem
		container: self model;
		buildWithSpec)
		parentNode: self;
		yourself