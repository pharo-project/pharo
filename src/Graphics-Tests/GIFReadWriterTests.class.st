Class {
	#name : #GIFReadWriterTests,
	#superclass : #TestCase,
	#category : #'Graphics-Tests'
}

{ #category : #'as yet unclassified' }
GIFReadWriterTests >> animatedColorFrames [
	"Responds with a collection of AnimatedImageFrames
	each containing equal-sized rectangles of green, red, 
	yellow. and black. When animated, these rectangles move
	into each other's positions, then back"
	| frames topLeft topRight bottomLeft bottomRight revFrames |
	frames := OrderedCollection new.
	topLeft := (0@0 extent: 200@200).
	topRight := (200@0 extent: 400@200).
	bottomLeft := (0@200 extent: 200@400).
	bottomRight := (200@200 extent: 400@400).
	
	0 to: 200 by: 20 do: [ :num |
		| newTopLeft newTopRight newBottomLeft newBottomRight canvas |
		newTopLeft := topLeft translateBy: (num@0).
		newTopRight := topRight translateBy: (0@num).
		newBottomLeft := (bottomLeft origin - (0@num)) corner: (bottomLeft extent - (0@num)).
		newBottomRight := (bottomRight origin - (num@0)) extent: (bottomLeft extent - (0@0)).
		canvas := FormCanvas extent: 400@400.
		canvas
			fillRectangle: newTopLeft color: Color green;
			fillRectangle: newTopRight color: Color red;
			fillRectangle: newBottomLeft color: Color yellow;
			fillRectangle: newBottomRight color: Color black.
		frames add: (AnimatedImageFrame new
			form: canvas form;
			disposal: #restoreBackground;
			delay: 60) ].

	revFrames := frames reversed.
	revFrames do: [ :f | frames add: f ].
	^ frames
]

{ #category : #tests }
GIFReadWriterTests >> testAnimatedColorsOutIn [
	"Ensure that the colored rectangles in the created
	animated gif are correct at different frames"
	| frames outBytes writer reader inBytes activeFrame |
	frames := self animatedColorFrames.
	
	"Write out the GIF bytes to the byte stream"
	outBytes := WriteStream on: (ByteArray new).
	writer := NeoGIFReadWriter on: outBytes.
	frames do: [ :f | writer nextPutFrame: f ].
	writer close.
	
	"Read the GIF byte stream back into a GIF"
	inBytes := ReadStream on: (outBytes contents).
	reader := NeoGIFReadWriter on: inBytes.
	reader
		readHeader;
		readBody.
	
	self assert: reader isAnimated equals: true.
	activeFrame := reader frames at: 1.
	self assert: (activeFrame form colorAt: 100@100) equals: Color green.
	self assert: (activeFrame form colorAt: 300@100) equals: Color red.
	self assert: (activeFrame form colorAt: 100@300) equals: Color yellow.
	self assert: (activeFrame form colorAt: 300@300) equals: Color black.
	activeFrame := reader frames at: 11.
	self assert: (activeFrame form colorAt: 100@100) equals: Color yellow.
	self assert: (activeFrame form colorAt: 300@100) equals: Color green.
	self assert: (activeFrame form colorAt: 100@300) equals: Color black.
	self assert: (activeFrame form colorAt: 300@300) equals: Color red.
	
]

{ #category : #tests }
GIFReadWriterTests >> testColorsFileOutIn [
	"Ensure that the colors that are written match up
	to the colors that are read in again"
	| canvas form outBytes inBytes writer reader |
	canvas := FormCanvas extent: 400@400.
	canvas
		fillRectangle: (0@0 extent: 200@200) color: Color green;
		fillRectangle: (200@0 extent: 400@200) color: Color red;
		fillRectangle: (0@200 extent: 200@400) color: Color yellow;
		fillRectangle: (200@200 extent: 400@400) color: Color black.
	form := canvas form.
	outBytes := 'testColorsFileOutIn.gif' asFileReference binaryWriteStream.
	writer := NeoGIFReadWriter on: outBytes.
	writer
		nextPutImage: form;
		close.
	inBytes := 'testColorsFileOutIn.gif' asFileReference binaryReadStream.
	reader := NeoGIFReadWriter on: inBytes.
	reader
		readHeader;
		readBody.
	'testColorsFileOutIn.gif' asFileReference delete.
	self assert: reader isAnimated equals: false.
	self assert: (reader form colorAt: 50@50) equals: Color green.
	self assert: (reader form colorAt: 250@50) equals: Color red.
	self assert: (reader form colorAt: 50@250) equals: Color yellow.
	self assert: (reader form colorAt: 250@250) equals: Color black.
]

{ #category : #tests }
GIFReadWriterTests >> testColorsOutIn [
	"Ensure that the colors that are written match up
	to the colors that are read in again"
	| canvas form outBytes inBytes writer reader |
	canvas := FormCanvas extent: 400@400.
	canvas
		fillRectangle: (0@0 extent: 200@200) color: Color green;
		fillRectangle: (200@0 extent: 400@200) color: Color red;
		fillRectangle: (0@200 extent: 200@400) color: Color yellow;
		fillRectangle: (200@200 extent: 400@400) color: Color black.
	form := canvas form.
	outBytes := WriteStream on: (ByteArray new).
	writer := NeoGIFReadWriter on: outBytes.
	writer
		nextPutImage: form;
		close.
	outBytes := outBytes contents.
	inBytes := ReadStream on: outBytes.
	reader := NeoGIFReadWriter on: inBytes.
	reader
		readHeader;
		readBody.
	self assert: reader isAnimated equals: false.
	self assert: (reader form colorAt: 50@50) equals: Color green.
	self assert: (reader form colorAt: 250@50) equals: Color red.
	self assert: (reader form colorAt: 50@250) equals: Color yellow.
	self assert: (reader form colorAt: 250@250) equals: Color black.
]
