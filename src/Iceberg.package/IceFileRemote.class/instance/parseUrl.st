private
parseUrl
	| matcher |
	matcher := 'file\://(.*)' asRegex.
	(matcher matches: url) 
		ifTrue: [ 
			path := (matcher subexpression: 2) asFileReference.
			projectName := path basename.
		]
		ifFalse: [ IceWrongUrl signal: 'Could not parse  "file:" url: ', url ]
