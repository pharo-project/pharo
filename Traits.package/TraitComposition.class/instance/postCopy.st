copying
postCopy
	super postCopy.
	transformations := transformations collect: [:each | each copy].