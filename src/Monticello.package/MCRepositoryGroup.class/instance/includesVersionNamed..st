interface
includesVersionNamed: aString
	" check for existing version name in parallel over all repositories "
	| results |

	results := Array new: self repositories size.
	
	self repositories doWithIndex: [:repository :index | 
		" fork of test for each repository "
		results 
			at: index 
			put: (repository includesVersionNamed: aString) ].

	" check if any repository included the given versionName already"
	^ results anySatisfy: [:result| result = true ]	