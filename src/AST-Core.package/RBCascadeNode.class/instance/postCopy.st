copying
postCopy
	super postCopy.
	self messages: (self messages collect: [ :each | each copy ])