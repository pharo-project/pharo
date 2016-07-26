accessing
nextImage
	bigEndian := EndianDetector isBigEndian.
	filtersSeen := Bag new.
	idatChunkStream := nil.
	transparentPixelValue := nil.
	unknownChunks := Set new.
	stream reset.
	stream binary.
	stream skip: 8.
	[stream atEnd] whileFalse: [self processNextChunk].
	"Set up our form"
	palette ifNotNil: 
			["Dump the palette if it's the same as our standard palette"

			palette = (StandardColors copyFrom: 1 to: palette size) 
				ifTrue: [palette := nil]].
	(depth <= 8 and: [palette notNil]) 
		ifTrue: 
			[form := ColorForm extent: width @ height depth: depth.
			form colors: palette]
		ifFalse: [form := Form extent: width @ height depth: depth].
	backColor ifNotNil: [form fillColor: backColor].
 	idatChunkStream 
 		ifNil: [ self error: 'image data is missing' ]
 		ifNotNil: [ self processIDATChunk ].
	unknownChunks isEmpty 
		ifFalse: 
			["Transcript show: ' ',unknownChunks asSortedCollection asArray printString."].
	self debugging 
		ifTrue: 
			[Transcript
				cr;
				show: 'form = ' , form printString.
			Transcript
				cr;
				show: 'colorType = ' , colorType printString.
			Transcript
				cr;
				show: 'interlaceMethod = ' , interlaceMethod printString.
			Transcript
				cr;
				show: 'filters = ' , filtersSeen sortedCounts asArray printString].
	^form