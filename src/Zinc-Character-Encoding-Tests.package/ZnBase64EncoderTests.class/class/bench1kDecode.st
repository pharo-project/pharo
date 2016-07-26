benchmarks
bench1kDecode
	"self bench1kDecode"
	
	| string alphabet |
	alphabet := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'. 
	string := String new: 1024 streamContents: [ :out |
		1024 timesRepeat: [ out nextPut: alphabet atRandom ] ].
	^ [ ZnBase64Encoder new decode: string ] bench