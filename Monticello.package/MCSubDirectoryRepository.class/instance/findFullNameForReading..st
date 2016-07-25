i/o
findFullNameForReading: aBaseName
	"Answer the latest version of aBaseName"
	^((directory glob: [ :each | each basename = aBaseName])
		sorted: [ :a :b | a modificationTime < b modificationTime ])
		first.