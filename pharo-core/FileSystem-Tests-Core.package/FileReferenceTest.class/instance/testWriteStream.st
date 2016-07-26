tests
testWriteStream
	| ref stream |
	ref := filesystem / 'plonk'.
	[stream := ref writeStream.]
		ensure: [stream ifNotNil: [stream close]]