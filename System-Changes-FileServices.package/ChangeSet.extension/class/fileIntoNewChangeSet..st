*System-Changes-FileServices
fileIntoNewChangeSet: fullName
	"File in all of the contents of the currently selected file, if any, into a new change set." 

	| fn ff |
	fullName ifNil: [^ self ].
	
	fn := (Smalltalk hasClassNamed: #GZipReadStream) 
		ifTrue: [(Smalltalk classNamed:  #GZipReadStream) uncompressedFileName: fullName]
		ifFalse: [fullName].
	[ ff := FileStream readOnlyFileNamed: fn.
	self newChangesFromStream: ff named: (fn  asFileReference basename)]
		ensure: [ff ifNotNil: [ff close]]