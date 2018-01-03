"
I implement a file format that compresses segment by segment to allow incremental writing and browsing.  Note that the file can only be written at the end.

Structure:
segmentFile		The actual compressed file.
segmentSize		This is the quantum of compression.  The virtual file is sliced up
				into segments of this size.
nSegments		The maximum number of segments to which this file can be grown.
endOfFile		The user's endOfFile pointer.
segmentTable	When a file is open, this table holds the physical file positions
				of the compressed segments.
segmentIndex	Index of the most recently accessed segment.

Inherited from ReadWriteStream...
collection		The segment buffer, uncompressed
position			This is the position *local* to the current segment buffer
readLimit		ReadLimit for the current buffer
writeLimit		WriteLimit for the current buffer

Great care must be exercised to distinguish between the position relative to the segment buffer and the full file position (and, or course, the segment file position ;-).

The implementation defaults to a buffer size of 20k, and a max file size of 34MB.  The format of the file is as follows:
	segmentSize		4 bytes
	nSegments		4 bytes
	endOfFile		4 bytes
	segmentTable	4 bytes * (nSegments+1)
	beginning of first compressed segment

It is possible to override the default allocation by sending the message #segmentSize:nSegments: immediately after opening a new file for writing, as follows:

	bigFile := (CompressedSourceStream on: (FileStream newFileNamed: 'biggy.stc'))
			segmentSize: 50000 maxSize: 200000000

The difference between segment table entries reveals the size of each compressed segment.  When a file is being written, it may lack the final segment, but any flush, position:, or close will force a dirty segment to be written.
"
Class {
	#name : #CompressedSegmentsStream,
	#superclass : #ReadWriteStream,
	#instVars : [
		'segmentFile',
		'segmentSize',
		'nSegments',
		'segmentTable',
		'segmentIndex',
		'dirty',
		'endOfFile'
	],
	#category : #'CompressedSegmentsStream-Core'
}

{ #category : #'compressed sources' }
CompressedSegmentsStream class >> canDecompressSourcesToDisk [

	^ (SourceFileArray sourcesFS root / (Smalltalk sourcesName asFileReference) base, 'stc') exists.
]

{ #category : #'compressed sources' }
CompressedSegmentsStream class >> compressSourcesInMemory [

	| originalSources originalSourcesReference compressedSources compressedSourcesReference segmentSize |
	
	segmentSize := 20000.
	originalSourcesReference := Smalltalk sourcesName asFileReference.
	originalSources := originalSourcesReference readStream reset binary.
	
	compressedSourcesReference := SourceFileArray sourcesFS root / originalSourcesReference base, 'stc'.
	compressedSourcesReference ensureDelete.

	compressedSources := (CompressedSegmentsStream 
		on: (compressedSourcesReference writeStream binary)) 
		segmentSize: segmentSize 
		maxSize: originalSources size.

	originalSources position: 0.
	[originalSources atEnd] whileFalse:
			[compressedSources nextPutAll: (originalSources next: segmentSize)].

	originalSources close.
	compressedSources close.
]

{ #category : #'compressed sources' }
CompressedSegmentsStream class >> decompressSourcesToDisk [

	| compressed ref exported |

	compressed := SourceFiles sourcesFileStream wrappedStream.
	compressed position: 0.
	ref := Smalltalk sourcesName asFileReference.
	ref ensureDelete.
	exported := (File openForWriteFileNamed: ref fullName).
	[compressed atEnd] whileFalse:
		[exported nextPutAll: (compressed next: 20000)].
	exported close.
	(SourceFileArray sourcesFS root / (Smalltalk sourcesName asFileReference) base, 'stc') ensureDelete.
	PharoFilesOpener default sourcesFileOrNil.
	
]

{ #category : #'instance creation' }
CompressedSegmentsStream class >> on: aFile [
	^ self basicNew openOn: aFile
]

{ #category : #access }
CompressedSegmentsStream >> atEnd [

	position >= readLimit ifFalse: [^ false].  "more in segment"
	^ self position >= endOfFile  "more in file"
]

{ #category : #'open/close' }
CompressedSegmentsStream >> close [
	self flush.
	segmentFile close
]

{ #category : #access }
CompressedSegmentsStream >> contentsOfEntireFile [
	| contents |
	self position: 0.
	contents := self next: self size.
	self close.
	^ contents
]

{ #category : #private }
CompressedSegmentsStream >> firstSegmentLoc [
	"First segment follows 3 header words and segment table"
	^ (3 + nSegments+1) * 4
]

{ #category : #access }
CompressedSegmentsStream >> flush [
	dirty ifTrue:
		["Write buffer, compressed, to file, and also write the segment offset and eof"
		self writeSegment].
]

{ #category : #access }
CompressedSegmentsStream >> next [

	position >= readLimit
		ifTrue: [^ (self next: 1) at: 1]
		ifFalse: [^ collection at: (position := position + 1)]
]

{ #category : #access }
CompressedSegmentsStream >> next: n [
	| str |
	n <= (readLimit - position) ifTrue:
		["All characters are available in buffer"
		str := collection copyFrom: position + 1 to: position + n.
		position := position + n.
		^ str].

	"Read limit could be segment boundary or real end of file"
	(readLimit + self segmentOffset) = endOfFile ifTrue:
		["Real end of file -- just return what's available"
		^ self next: readLimit - position].

	"Read rest of segment.  Then (after positioning) read what remains"
	str := self next: readLimit - position.
	self position: self position.
	^ str , (self next: n - str size)

]

{ #category : #access }
CompressedSegmentsStream >> nextPut: char [
	"Slow, but we don't often write, and then not a lot"
	self nextPutAll: {char}.
	^ char
]

{ #category : #access }
CompressedSegmentsStream >> nextPutAll: str [
	| n nInSeg |
	n := str size.
	n <= (writeLimit - position) ifTrue:
		["All characters fit in buffer"
		collection replaceFrom: position + 1 to: position + n with: str.
		dirty := true.
		position := position + n.
		readLimit := readLimit max: position.
		endOfFile := endOfFile max: self position.
		^ str].

	"Write what fits in segment.  Then (after positioning) write what remains"
	nInSeg := writeLimit - position.
	nInSeg = 0
		ifTrue: [self position: self position.
				self nextPutAll: str]
		ifFalse: [self nextPutAll: (str first: nInSeg).
				self position: self position.
				self nextPutAll: (str allButFirst: nInSeg)]
	

]

{ #category : #'open/close' }
CompressedSegmentsStream >> openOn: aFile [
	"Open the receiver."
	segmentFile := aFile.
	segmentFile binary.
	segmentFile size > 0
	ifTrue:
		[self readHeaderInfo.  "If file exists, then read the parameters"]
	ifFalse:
		[self segmentSize: 20000 maxSize: 34000000.  "Otherwise write default values"]
]

{ #category : #'open/close' }
CompressedSegmentsStream >> openReadOnly [

	segmentFile openReadOnly
]

{ #category : #access }
CompressedSegmentsStream >> position [

	^ position + self segmentOffset
]

{ #category : #access }
CompressedSegmentsStream >> position: newPosition [
	| compressedBuffer newSegmentIndex |
	newPosition > endOfFile ifTrue:
		[self error: 'Attempt to position beyond the end of file'].
	newSegmentIndex := (newPosition // segmentSize) + 1.
	newSegmentIndex ~= segmentIndex ifTrue:
		[self flush.
		segmentIndex := newSegmentIndex.
		newSegmentIndex > nSegments ifTrue:
			[self error: 'file size limit exceeded'].
		segmentFile position: (segmentTable at: segmentIndex).
		(segmentTable at: segmentIndex+1) = 0
			ifTrue:
			[newPosition ~= endOfFile ifTrue:
				[self error: 'Internal logic error'].
			collection size = segmentSize ifFalse:
				[self error: 'Internal logic error'].
			"just leave garbage beyond end of file"]
			ifFalse:
			[compressedBuffer := segmentFile next: ((segmentTable at: segmentIndex+1) - (segmentTable at: segmentIndex)). 
			collection := (GZipReadStream on: compressedBuffer) contents asByteArray].
		readLimit := collection size min: endOfFile - self segmentOffset].
	position := newPosition \\ segmentSize.
	
]

{ #category : #'open/close' }
CompressedSegmentsStream >> readHeaderInfo [
	| valid a b |
	segmentFile position: 0.
	segmentSize := segmentFile nextNumber: 4.
	nSegments := segmentFile nextNumber: 4.
	endOfFile := segmentFile nextNumber: 4.
	segmentFile size < (nSegments+1 + 3 * 4) ifTrue: "Check for reasonable segment info"
		[self error: 'This file is not in valid compressed source format'].
	segmentTable := (1 to: nSegments+1) collect: [:x | segmentFile nextNumber: 4].
	segmentTable first ~= self firstSegmentLoc ifTrue:
		[self error: 'This file is not in valid compressed source format'].
	valid := true.
	1 to: nSegments do:  "Check that segment offsets are ascending"
		[:i | a := segmentTable at: i.  b := segmentTable at: i+1.
		(a = 0 and: [b ~= 0]) ifTrue: [valid := false].
		(a ~= 0 and: [b ~= 0]) ifTrue: [b <= a ifTrue: [valid := false]]].
	valid ifFalse:
		[self error: 'This file is not in valid compressed source format'].
	dirty := false.
	self position: 0.
]

{ #category : #'open/close' }
CompressedSegmentsStream >> readOnlyCopy [

	^ self class on: segmentFile readOnlyCopy
]

{ #category : #private }
CompressedSegmentsStream >> segmentOffset [

	^ segmentIndex - 1 * segmentSize
]

{ #category : #private }
CompressedSegmentsStream >> segmentSize: segSize maxSize: maxSize [
	"Note that this method can be called after the initial open, provided that no
	writing has yet taken place.  This is how to override the default segmentation."
	self size = 0 ifFalse: [self error: 'Cannot set parameters after the first write'].
	segmentFile position: 0.
	segmentFile nextNumber: 4 put: (segmentSize := segSize).
	segmentFile nextNumber: 4 put: (nSegments := maxSize // segSize + 2).
	segmentFile nextNumber: 4 put: (endOfFile := 0).
	segmentTable := Array new: nSegments+1 withAll: 0.
	segmentTable at: 1 put: self firstSegmentLoc.  "Loc of first segment, always."
	segmentTable do: [:i | segmentFile nextNumber: 4 put: i].
	segmentIndex := 1.
	collection := ByteArray new: segmentSize.
	writeLimit := segmentSize.
	readLimit := 0.
	position := 0.
	endOfFile := 0.
	self writeSegment.

]

{ #category : #access }
CompressedSegmentsStream >> size [
	^ endOfFile ifNil: [0]
]

{ #category : #private }
CompressedSegmentsStream >> writeSegment [
	"The current segment must be the last in the file."
	| compressedSegment |
	segmentFile position: (segmentTable at: segmentIndex).
	compressedSegment := ByteArray streamContents:
		[:strm | (GZipWriteStream on: strm) nextPutAll: collection; close].
	segmentFile nextPutAll: compressedSegment.
	segmentTable at: segmentIndex + 1 put: segmentFile position.

	segmentFile position: 2 * 4.
	segmentFile nextNumber: 4 put: endOfFile.
	segmentFile position: (segmentIndex + 3) * 4.
	segmentFile nextNumber: 4 put: (segmentTable at: segmentIndex + 1).
	dirty := false
]
