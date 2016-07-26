accessing
removeTemporaryNamed: aName 
	temporaries := temporaries reject: [:each | each name = aName]