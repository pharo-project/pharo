copying
postCopy
	super postCopy.
	self contents: (self contents collect: [ :each | each copy ])