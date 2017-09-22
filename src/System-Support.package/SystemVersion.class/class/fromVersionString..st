instance creation
fromVersionString: aString
	| matcher instance minor major |
	matcher := '(([A-Z][a-zA-Z]*)[-]*([0-9]+)(.([0-9]+))*)(([+](([a-zA-Z]*)([.]*build.([0-9]+)(.sha.([0-9a-z]+))*)*))*)' asRegex.
	(matcher matches: aString)
		ifFalse: [ ^ self error: 'Not a version string' ].
	
	major := 	(matcher subexpression: 4) asNumber.
	minor := (matcher subexpression: 6) isString ifTrue: [ (matcher subexpression: 6) asNumber ] ifFalse: [ 0 ]. 	
	instance := self major: major minor: minor.
	instance type: (matcher subexpression: 3).
	(matcher subexpression: 10) isString ifTrue: [ instance suffix: (matcher subexpression: 10) ].
	(matcher subexpression: 12) isString ifTrue: [ instance build: (matcher subexpression:  12) asNumber ].
	(matcher subexpression: 14) isString ifTrue: [ instance commitHash: (matcher subexpression:  14) ].
	^ instance