benchmarks
bench1kEncode
	"self bench1kEncode"
	
	| bytes |
	bytes := ByteArray new: 1024 streamContents: [ :out |
		1024 timesRepeat: [ out nextPut: 256 atRandom - 1 ] ].
	^ [ ZnBase64Encoder new encode: bytes ] bench