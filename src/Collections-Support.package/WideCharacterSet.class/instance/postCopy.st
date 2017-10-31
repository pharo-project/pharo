copying
postCopy
	super postCopy.
	byteArrayMap := byteArrayMap copy.
	map := map collect: [:each | each copy]