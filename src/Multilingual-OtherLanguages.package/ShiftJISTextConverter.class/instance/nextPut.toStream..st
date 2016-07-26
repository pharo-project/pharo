conversion
nextPut: aCharacter toStream: aStream 
	| value leadingChar aChar |
	aStream isBinary ifTrue: [^aCharacter storeBinaryOn: aStream].
	value := aCharacter charCode.
	(value between: 16rFF61 and: 16rFF9F) ifTrue: [
		aStream basicNextPut: (self sjisKatakanaFor: value).
		^ aStream].
	aChar := JISX0208 charFromUnicode: value.
	aChar ifNil: [^ aStream].
	value := aChar charCode.
	leadingChar := aChar leadingChar.
	leadingChar = 0 ifTrue: [
		aStream basicNextPut: (Character value: value).
		^ aStream.
	].
	leadingChar == self leadingChar ifTrue: [
		| upper lower | 
		upper := value // 94 + 33.
		lower := value \\ 94 + 33.
		upper \\ 2 = 1 ifTrue: [
			upper := upper + 1 / 2 + 112.
			lower := lower + 31
		] ifFalse: [
			upper := upper / 2 + 112.
			lower := lower + 125
		].
		upper >= 160 ifTrue: [upper := upper + 64].
		lower >= 127 ifTrue: [lower := lower + 1].
		aStream basicNextPut: (Character value: upper).
		aStream basicNextPut: (Character value: lower).
		^ aStream
	].
