*GT-SpotterExtensions-Core
spotterPreviewPngIn: aComposite
	<spotterPreview: 10>
	
	(self mimeTypes notNil and: [ 
		self mimeTypes first matches: ZnMimeType imagePng ]) 
		ifFalse: [ ^ self ].
	aComposite custom: 
		((self binaryReadStreamDo: [ :stream | PNGReadWriter formFromStream: stream ])
			asAlphaImageMorph asBrick margin: 20)