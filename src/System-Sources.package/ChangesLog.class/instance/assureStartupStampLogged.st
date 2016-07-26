private
assureStartupStampLogged
	"If there is a startup stamp not yet actually logged to disk, do it now."

	startupStamp ifNil: [ ^ self ].

	SourceFiles changesWriteStreamDo: [ :changesFile |
		changesFile
			cr; cr;
			nextChunkPut: startupStamp asString;
			cr.
		startupStamp := nil ].

	SourceFiles forceChangesToDisk