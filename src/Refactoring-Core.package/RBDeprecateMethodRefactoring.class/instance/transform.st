transforming
transform
	| protocol deprecatedSource |
	protocol := (class methodFor: newSelector) protocols.
	deprecatedSource := self formOldName , String cr , String tab
		,
			'self 
		deprecated: ''Use ' , newSelector asString
		,
			' instead''
		on: 	''' , Date today asString
		,
			''' 
		in: ''' , SystemVersion current imageVersionString
		,
			'''.
	^ self ' , self formNewName.
	(model classNamed: class realClass name) compile: deprecatedSource classified: protocol