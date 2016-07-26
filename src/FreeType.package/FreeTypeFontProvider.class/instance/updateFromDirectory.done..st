loading and updating
updateFromDirectory: aDirectory done: aSet
	"get info from fonts in aDirectory"
	
	(aSet includes: aDirectory) ifTrue: [ ^ self ].
	aSet add: aDirectory.
	aDirectory files 
		do: [ :each |
			"SUSE 10.2 has lots of files ending .gz that aren't fonts.
			We skip them to save time'"
			((each basename beginsWith: '.' ) or: [ each basename asLowercase endsWith:' .gz' ])
				ifFalse: [ self updateFromFile: each ]]
		displayingProgress: 'Loading fonts in ', aDirectory fullName.
		
	aDirectory directories do: [ :each |
		self updateFromDirectory: each done: aSet ].