do-its
compileSelectionFor: anObject in: evalContext
	
	^ self compile: self selectionAsStream for: anObject in: evalContext