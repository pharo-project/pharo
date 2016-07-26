execute
filterBlock
	| project packageNames |
	
	project := target.
	packageNames := project packages collect: [ :package | package name ].
	packageNames add: project configurationClass name asString.
	^ [ :workingCopy | packageNames includes: workingCopy package name ]
