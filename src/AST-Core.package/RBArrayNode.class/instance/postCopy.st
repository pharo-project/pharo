copying
postCopy
	super postCopy.
	self statements: (self statements collect: [ :each | each copy ])