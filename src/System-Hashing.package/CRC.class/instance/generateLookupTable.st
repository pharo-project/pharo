private-tables
generateLookupTable
	"lookup tables have 256 entries"
	^ Array 
		new: 256
		streamContents: [ :aStream | self printLookupTableOn: aStream ]