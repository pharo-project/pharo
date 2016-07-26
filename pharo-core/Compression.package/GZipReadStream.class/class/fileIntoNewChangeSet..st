filein/out
fileIntoNewChangeSet: fullFileName
	"FileIn the contents of a gzipped file"

	| zipped unzipped csClass |
	csClass := Smalltalk globals at: #ChangeSet ifAbsent: [ ^ self ].
	zipped := self on: (FileStream readOnlyFileNamed: fullFileName).
	unzipped := MultiByteBinaryOrTextStream with: zipped contents asString.
	unzipped reset.
	csClass 
		newChangesFromStream: unzipped 
		named: fullFileName asFileReference basename