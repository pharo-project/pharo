copying
postCopy
	super postCopy.
	self receiver: self receiver copy.
	self arguments: (self arguments collect: [ :each | each copy ])