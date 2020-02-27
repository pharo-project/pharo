Extension { #name : #ZipArchive }

{ #category : #'*Deprecated80' }
ZipArchive >> writeTo: stream prependingFileNamed: aFileName [
	| prepended buffer |
	self deprecated: 'use ... writeTo:prependingFile: instead'.
	
	prepended := File openForReadFileNamed: aFileName.
	buffer := ByteArray new: (prepended size min: 32768).
	[ prepended atEnd ]
		whileFalse: [ | bytesRead |
			bytesRead := prepended
				readInto: buffer
				startingAt: 1
				count: buffer size.
			stream next: bytesRead putAll: buffer startingAt: 1 ].
	self writeTo: stream.

]

{ #category : #'*Deprecated80' }
ZipArchive >> writeToFileNamed: aFileReferenceOrFileName prepending: aString [
	
	self deprecated: 'use ... writeTo:prepending: instead'.
	
	"Catch attempts to overwrite existing zip file"
	(self canWriteToFileNamed: aFileReferenceOrFileName)
		ifFalse: [ ^ ZipArchiveError signal: (aFileReferenceOrFileName asString, ' is needed by one or more members in this archive') ].
	
	aFileReferenceOrFileName asFileReference
		binaryWriteStreamDo: [ :stream | self writeTo: stream prepending: aString ]	
]

{ #category : #'*Deprecated80' }
ZipArchive >> writeToFileNamed: aFileReferenceOrFileName prependingFileNamed: anotherFileReferenceOrFileName [
	
	self deprecated: 'use ... writeTo:prependingFile: instead'.
	
	"Catch attempts to overwrite existing zip file"
	(self canWriteToFileNamed: aFileReferenceOrFileName)
		ifFalse: [ ^ZipArchiveError signal: (aFileReferenceOrFileName asString, ' is needed by one or more members in this archive') ].
	
	aFileReferenceOrFileName 
		binaryReadStreamDo: [ :stream | self writeTo: stream prependingFile: anotherFileReferenceOrFileName ]	
]
