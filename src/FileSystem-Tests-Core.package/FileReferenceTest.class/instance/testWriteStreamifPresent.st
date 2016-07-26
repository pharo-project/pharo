tests
testWriteStreamifPresent
	| ref stream |
	ref := filesystem / 'plonk'.
	[stream := ref writeStreamIfPresent: [self signalFailure: 'Should not reach here']]
		ensure: [stream ifNotNil: [stream close]]