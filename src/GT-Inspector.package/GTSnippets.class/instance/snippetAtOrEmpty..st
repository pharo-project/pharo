accessing
snippetAtOrEmpty: anObject
	^ self snippets 
		at: anObject 
		ifAbsent: [ '' ]