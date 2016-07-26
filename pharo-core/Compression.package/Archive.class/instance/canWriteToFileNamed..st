archive operations
canWriteToFileNamed: aFileName
	"Catch attempts to overwrite existing zip file"
	^(members anySatisfy: [ :ea | ea usesFileNamed: aFileName ]) not.
