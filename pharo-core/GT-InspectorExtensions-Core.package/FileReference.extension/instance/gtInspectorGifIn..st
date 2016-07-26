*GT-InspectorExtensions-Core
gtInspectorGifIn: composite
	<gtInspectorPresentationOrder: 0>
	composite morph
			title: 'Picture';
			display: [ self binaryReadStreamDo: [ :stream | GIFReadWriter formFromStream: stream ] ];
			when: [ self isFile and: [self mimeTypes notNil and: [ self mimeTypes first matches: ZnMimeType imageGif ] ] ]