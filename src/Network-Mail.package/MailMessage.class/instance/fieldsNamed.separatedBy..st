fields
fieldsNamed: aString  separatedBy: separationString
	"return all fields with the specified name, concatenated together with separationString between each element.  Return an empty string if no fields with the specified name are present"
	| matchingFields |
	matchingFields := self fieldsNamed: aString ifAbsent: [ ^'' ].
	^String streamContents: [ :str |
		matchingFields
			do: [ :field | str nextPutAll: field mainValue ]
			separatedBy: [ str nextPutAll: separationString ]].
