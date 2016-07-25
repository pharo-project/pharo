conversion
nextPut: aCharacter toStream: aStream 
	| leadingChar nBytes mask shift ucs2code |
	aStream isBinary ifTrue: [^aCharacter storeBinaryOn: aStream].
	leadingChar := aCharacter leadingChar.
	(leadingChar = 0 and: [aCharacter asciiValue < 128]) ifTrue: [
		aStream basicNextPut: aCharacter.
		^ aStream.
	].

	"leadingChar > 3 ifTrue: [^ aStream]."

	ucs2code := aCharacter asUnicode.
	ucs2code ifNil: [^ aStream].

	nBytes := ucs2code highBit + 3 // 5.
	mask := #(128 192 224 240 248 252 254 255) at: nBytes.
	shift := nBytes - 1 * -6.
	aStream basicNextPut: (Character value: (ucs2code bitShift: shift) + mask).
	2 to: nBytes do: [:i | 
		shift := shift + 6.
		aStream basicNextPut: (Character value: ((ucs2code bitShift: shift) bitAnd: 63) + 128).
	].

	^ aStream.
