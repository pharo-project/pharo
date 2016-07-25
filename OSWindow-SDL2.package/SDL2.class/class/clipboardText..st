clipboard
clipboardText: text
	| encoded encoder |
	encoded := ByteArray streamContents: [ :stream |
		encoder := ZnUTF8Encoder new.
		encoder next: text size putAll: text startingAt: 1 toStream: stream.
		stream nextPut: 0 ].
	self primClipboardText: encoded