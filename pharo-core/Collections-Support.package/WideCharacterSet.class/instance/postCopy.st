copying
postCopy
	super postCopy.
	map := map collect: [:each | each copy]