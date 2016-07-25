private-encoding
encodeChar: aChar to: aStream

	aChar = Character space
		ifTrue: [^ aStream nextPut: $_].
	((aChar asciiValue between: 32 and: 127) and: [('?=_' includes: aChar) not])
		ifTrue: [^ aStream nextPut: aChar].
	aStream nextPut: $=;
		nextPut: (Character digitValue: aChar asciiValue // 16);
		nextPut: (Character digitValue: aChar asciiValue \\ 16)
