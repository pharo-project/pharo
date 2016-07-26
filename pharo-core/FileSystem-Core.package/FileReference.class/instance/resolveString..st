resolving
resolveString: aString 
	| thePath |
	thePath := filesystem pathFromString: aString.
	^ filesystem referenceTo: (path resolve: thePath)