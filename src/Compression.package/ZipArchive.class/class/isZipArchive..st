file format
isZipArchive: aStreamOrFileName
	"Answer whether the given filename represents a valid zip file."

	| stream eocdPosition |
	stream := aStreamOrFileName isStream
		ifTrue: [aStreamOrFileName]
		ifFalse: [StandardFileStream oldFileNamed: aStreamOrFileName].
	stream ifNil: [^ false].
	"nil happens sometimes somehow"
	stream size < 22 ifTrue: [^ false].
	stream binary.
	eocdPosition := self findEndOfCentralDirectoryFrom: stream.
	stream ~= aStreamOrFileName ifTrue: [stream close].
	^ eocdPosition > 0