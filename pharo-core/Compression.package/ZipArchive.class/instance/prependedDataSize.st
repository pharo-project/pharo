accessing
prependedDataSize
	"Answer the size of whatever data exists before my first member.
	Assumes that I was read from a file or stream (i.e. the first member is a ZipFileMember)"
	^members isEmpty
		ifFalse: [ members first localHeaderRelativeOffset ]
		ifTrue: [ centralDirectoryOffsetWRTStartingDiskNumber ]