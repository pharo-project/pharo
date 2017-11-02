Extension { #name : #ReadWriteStream }

{ #category : #'*Compression' }
ReadWriteStream >> asUnZippedStream [
	| isGZip outputStream first strm archive |
	"Decompress this file if needed, and return a stream.  No file is written.  File extension may be .gz or anything else.  Also works on archives (.zip, .gZip)."
	strm := self binary.
	strm isZipArchive
		ifTrue: [ 
			archive := ZipArchive new readFrom: strm.
			archive members
				detect: [ :any | any fileName asLowercase endsWith: '.ttf' ]
				ifFound: [ :which | 
					strm := which contentStream.
					archive close ]
				ifNone: [ 
					archive close.
					^ self error: 'Can''t find .ttf file in archive' ] ].
	first := strm next.
	isGZip := strm next * 256 + first = GZipConstants gzipMagic.
	strm skip: -2.
	isGZip
		ifTrue: [ 
			outputStream := (MultiByteBinaryOrTextStream with: (GZipReadStream on: strm) upToEnd) reset.
			strm close ]
		ifFalse: [ outputStream := strm ].
	^ outputStream
]

{ #category : #'*Compression' }
ReadWriteStream >> isZipArchive [
	"Determine if this appears to be a valid Zip archive"
	| sig |
	self binary.
	sig := self next: 4.
	self position: self position - 4. "rewind"
	^ZipArchive validSignatures includes: sig
]
