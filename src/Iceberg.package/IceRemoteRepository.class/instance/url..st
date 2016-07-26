accessing
url: anObject
	url := anObject.
	(url beginsWith: 'git@') 
		ifTrue: [ self parseScpUrl ]
		ifFalse: [ self error: 'Remote repository scheme not supported: ', url ]