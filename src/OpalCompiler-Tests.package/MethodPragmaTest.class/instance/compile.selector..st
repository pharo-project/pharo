utilities
compile: aString selector: aSelector
	self class 
		compileSilently: aSelector , String lf , aString
		classified: self methodCategory.
	^ self class >> aSelector.