accessing
nextImage
	"Read in the next GIF image from the stream. Read it all into
memory first for speed."
	| f thisImageColorTable |
	stream class == ReadWriteStream ifFalse: 
		[ stream binary.
		self on: (ReadWriteStream with: stream contentsOfEntireFile) ].
	localColorTable := nil.
	self readHeader.
	f := self readBody.
	self close.
	f == nil ifTrue: [ ^ self error: 'corrupt GIF file' ].
	thisImageColorTable := localColorTable ifNil: [ colorPalette ].
	transparentIndex ifNotNil: 
		[ transparentIndex + 1 > thisImageColorTable size ifTrue: 
			[ thisImageColorTable := thisImageColorTable 
				forceTo: transparentIndex + 1
				paddingWith: Color white ].
		thisImageColorTable 
			at: transparentIndex + 1
			put: Color transparent ].
	f colors: thisImageColorTable.
	^ f