private
parseScpUrl
	| matcher |
	matcher := '(git@)?([\w.]+)\:([\w]+)/([\w\-]+)\.git/?.*' asRegex.
	(matcher matches: url) 
		ifTrue: [ 
			host := matcher subexpression: 3.
			owner := matcher subexpression: 4.
			projectName := matcher subexpression: 5.
		]
		ifFalse: [ IceWrongUrl signal: 'Could not parse scp url: ', url ]
			

	
	

