initialization
reset
	remoteStringMap := IdentityDictionary new.
	newChangesFile := self temporaryFile.
	
	"Keep a copy of the source streams for performance"
	sourceStreams := Array
			with: PharoFilesOpener default sourcesFileOrNil
			with: PharoFilesOpener default changesFileOrNil