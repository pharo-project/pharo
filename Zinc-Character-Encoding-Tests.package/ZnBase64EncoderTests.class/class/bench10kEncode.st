benchmarks
bench10kEncode
	"self bench10kEncode"
	
	| bytes |
	bytes := ByteArray new: 10240 streamContents: [ :out |
		10240 timesRepeat: [ out nextPut: 256 atRandom - 1 ] ].
	^ [ ZnBase64Encoder new encode: bytes ] bench