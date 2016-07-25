benchmarks
bench10kDecode
	"self bench10kDecode"
	
	| string alphabet |
	alphabet := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'. 
	string := String new: 10240 streamContents: [ :out |
		10240 timesRepeat: [ out nextPut: alphabet atRandom ] ].
	^ [ ZnBase64Encoder new decode: string ] bench