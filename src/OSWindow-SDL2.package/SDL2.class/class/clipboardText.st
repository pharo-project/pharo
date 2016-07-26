clipboard
clipboardText
	| text len data |
	text := self primClipboardText.
	len := self strlen: text.
	data := ByteArray new: len.
	LibC memCopy: text to: data size: len.
	^ ZnUTF8Encoder new decodeBytes: data.