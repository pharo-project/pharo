instance creation
browseVersionsOf: method class: aClass meta: meta category: msgCategory selector: aSelector

	self setRGMethodFrom: method.
	^ self buildBrowser