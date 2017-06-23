private
parseUrl
	| matcher |
	matcher := '(ssh\://)?(git@)?([\w.]+)(\:|/)/?([\w\-]+)/([\w\-]+)(\.git)?' asRegex.
	(matcher matches: url) 
		ifTrue: [
			host := matcher subexpression: 4.
			owner := matcher subexpression: 6.
			projectName := matcher subexpression: 7.
		]
		ifFalse: [ IceWrongUrl signal: 'Could not parse URL: ', url ]
