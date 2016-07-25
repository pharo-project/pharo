utility
suffixOf: aName
	"Answer the file extension of the given file"
	^ aName
		ifNil: ['']
		ifNotNil: [ aName asFileReference extension asLowercase]