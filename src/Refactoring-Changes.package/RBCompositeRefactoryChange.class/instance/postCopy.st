copying
postCopy
	super postCopy.
	changes := changes collect: [ :each | each copy ]