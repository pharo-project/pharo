accessing
hashStream: aPositionableStream

	| startPosition buf bitLength |
	self initialize.

	aPositionableStream atEnd ifTrue: [
		buf := ByteArray new: 64.
		buf at: 1 put: 128.
		self processBuffer: buf.
		^self finalValue ].

	startPosition := aPositionableStream position.
	[aPositionableStream atEnd] whileFalse: [
		buf := aPositionableStream next: 64 into: (ByteArray new: 64).
		(aPositionableStream atEnd not and: [buf size = 64])
			ifTrue: [self processBuffer: buf]
			ifFalse: [
				bitLength := (aPositionableStream position - startPosition) * 8.
				self processFinalBuffer: buf bitLength: bitLength]].

	^ self finalValue