utilities
zipEntryContents: aZipFileMember
	"Try to get the correct encoding for UTF8."

	^ ReadStream on: aZipFileMember contents convertFromSystemString