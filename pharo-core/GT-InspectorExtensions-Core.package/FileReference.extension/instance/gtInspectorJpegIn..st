*GT-InspectorExtensions-Core
gtInspectorJpegIn: composite
	<gtInspectorPresentationOrder: 0>
	composite morph
			title: 'Picture';
			display: [ self binaryReadStreamDo: [ :stream | ImageReadWriter formFromStream: stream ] ];
			when: [ self isFile and: [self mimeTypes notNil and: [ self mimeTypes first matches: ZnMimeType imageJpeg ] ] ]