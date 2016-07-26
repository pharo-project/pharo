printing
printOn: aStream indent: level 
	variable printOn: aStream indent: level.
	aStream nextPutAll: ' := '.
	value printOn: aStream indent: level + 2