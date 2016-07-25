accessing
allImages
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