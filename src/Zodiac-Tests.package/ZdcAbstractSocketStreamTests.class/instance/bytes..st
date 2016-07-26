accessing
bytes: size
	| count data pattern |
	count := 0.
	pattern := (0 to: 255) asByteArray.
	data := ByteArray streamContents: [ :stream |
		[ count < size ] whileTrue: [
			stream nextPutAll: pattern.
			count := count + pattern size ] ].
	^ data copyFrom: 1 to: size     