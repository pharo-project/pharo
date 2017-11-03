"
Read an animated GIF file.

Example: open all images of an animated GIF file in a Morphic window

gifPath :=  '/path/to/my_animated.gif'.
forms := (AnimatedGIFReadWriter formsFromFileNamed: gifPath) forms.
content := UITheme builder newRow: (forms collect: [:aForm| UITheme builder newImage: aForm]).
content openInWindowLabeled: 'Content of ', gifPath.
"
Class {
	#name : #AnimatedGIFReadWriter,
	#superclass : #GIFReadWriter,
	#instVars : [
		'forms',
		'delays',
		'comments'
	],
	#category : #Graphics-Files
}

{ #category : #'image reading/writing' }
AnimatedGIFReadWriter class >> formsFromFileNamed: fileName [ 
	| stream |
	stream := FileStream readOnlyFileNamed: fileName.
	^ self formsFromStream: stream
]

{ #category : #'image reading/writing' }
AnimatedGIFReadWriter class >> formsFromStream: stream [ 
	| reader |
	reader := self new on: stream reset.
	Cursor read showWhile: 
		[ reader allImages.
		reader close ].
	^ reader
]

{ #category : #'image reading/writing' }
AnimatedGIFReadWriter class >> typicalFileExtensions [
	"Answer a collection of file extensions (lowercase) which files that I can read might commonly have"
	^#('gif')
]

{ #category : #'image reading/writing' }
AnimatedGIFReadWriter class >> wantsToHandleGIFs [
	^true
]

{ #category : #accessing }
AnimatedGIFReadWriter >> allImages [
	| body colorTable |
	stream class == ReadWriteStream ifFalse: 
		[ stream binary.
		self on: (ReadWriteStream with: stream contentsOfEntireFile) ].
	localColorTable := nil.
	forms := OrderedCollection new.
	delays := OrderedCollection new.
	comments := OrderedCollection new.
	self readHeader.
	[ (body := self readBody) isNil ] whileFalse: 
		[ colorTable := localColorTable ifNil: [ colorPalette ].
		transparentIndex ifNotNil: 
			[ transparentIndex + 1 > colorTable size ifTrue: 
				[ colorTable := colorTable 
					forceTo: transparentIndex + 1
					paddingWith: Color white ].
			colorTable 
				at: transparentIndex + 1
				put: Color transparent ].
		body colors: colorTable.
		forms add: body.
		delays add: delay ].
	^ forms
]

{ #category : #private }
AnimatedGIFReadWriter >> comment: aString [
	comments add: aString
]

{ #category : #accessing }
AnimatedGIFReadWriter >> delays [
	^ delays
]

{ #category : #accessing }
AnimatedGIFReadWriter >> forms [
	^ forms
]

{ #category : #private-decoding }
AnimatedGIFReadWriter >> readBitData [
	| form |
	form := super readBitData.
	form offset: offset.
	^form
]
