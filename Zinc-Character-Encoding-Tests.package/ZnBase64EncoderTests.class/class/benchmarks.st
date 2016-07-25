benchmarks
benchmarks
	"self benchmarks"
	
	^ (#( bench1kEncode bench1kDecode bench10kEncode bench10kDecode )
		 collect: [ :each | each -> (self perform: each) ]) asDictionary