accessing
snippetAt: anObject
	^ self snippets 
		at: anObject 
		ifAbsent: [ 'self' ]