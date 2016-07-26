copying
postCopy
	super postCopy.
	self arguments: (self arguments collect: [ :each | each copy ])