fields
fieldsNamed: aString ifAbsent: aBlock
	"return a list of all fields with the given name"
	^fields at: aString asLowercase ifAbsent: aBlock