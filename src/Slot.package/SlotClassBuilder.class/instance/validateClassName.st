private validating
validateClassName
	"Validate the new class name. Raise warning if invalid."
	name
		detect: [ :c | (c isAlphaNumeric or: [ c = $_ ]) not ]
		ifFound: [ :c | InvalidGlobalName signal: 'Invalid character: ''' , c printString , '''' for: name ].
	name first canBeGlobalVarInitial
		ifFalse: [ InvalidGlobalName signal: 'Class name does not start with a capital letter' for: name ].
	name = #NameOfSubclass 
		ifTrue: [ InvalidGlobalName signal: 'NameOfSubclass is not a valid class name' for: name ].