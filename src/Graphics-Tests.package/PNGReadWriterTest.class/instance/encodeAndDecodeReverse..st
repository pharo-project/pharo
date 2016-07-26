helpers
encodeAndDecodeReverse: original
	"Make sure that the given form is encoded and decoded correctly"
	| stream bytes decoded reversed |
	fileName := 'testReverse', original depth printString,'.png'.
	self assert: original class == Form. "won't work with ColorForm"
	"Switch pixel order"
	reversed := Form extent: original extent depth: original depth negated.
	original displayOn: reversed.
	self assert: original width = reversed width.
	self assert: original height = reversed height.
	self assert: original depth = reversed depth.
	self deny: original nativeDepth = reversed nativeDepth.
	original depth = 32
		ifTrue:[self assert: original bits = reversed bits]
		ifFalse:[self deny: original bits = reversed bits].

	"encode"
	stream := ByteArray new writeStream.
	(PNGReadWriter on: stream) nextPutImage: reversed; close.
	bytes := stream contents.
	self writeEncoded: bytes.

	"decode"
	stream := bytes readStream.
	decoded := (PNGReadWriter new on: stream) nextImage.
	decoded display.

	"compare"
	self assert: original width = decoded width.
	self assert: original height = decoded height.
	self assert: original depth = decoded depth.
	self assert: original bits = decoded bits.
	self assert: original class == decoded class.
	(original isColorForm) ifTrue:[
		original colors with: decoded colors do:[:c1 :c2| | maxErr |
			"we must round here due to encoding errors"
			maxErr := 1. "max. error for 8bit rgb component"
			self assert: ((c1 red * 255) truncated - (c2 red * 255) truncated) abs <= maxErr.
			self assert: ((c1 green * 255) truncated - (c2 green * 255) truncated) abs <= maxErr.
			self assert: ((c1 blue * 255) truncated - (c2 blue * 255) truncated) abs <= maxErr.
			self assert: ((c1 alpha * 255) truncated - (c2 alpha * 255) truncated) abs <= maxErr.
		].
	].
	self deleteFile.