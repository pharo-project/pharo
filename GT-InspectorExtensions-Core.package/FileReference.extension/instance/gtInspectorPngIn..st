*GT-InspectorExtensions-Core
gtInspectorPngIn: composite
	<gtInspectorPresentationOrder: 0>
	composite morph
			title: 'Picture';
			display: [ self binaryReadStreamDo: [ :stream | PNGReadWriter formFromStream: stream ] ];
			when: [ self isFile and: [ self mimeTypes notNil and: [ self mimeTypes first matches: ZnMimeType imagePng ] ] ]