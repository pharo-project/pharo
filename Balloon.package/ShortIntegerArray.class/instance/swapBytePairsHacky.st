objects from disk
swapBytePairsHacky

	"The implementation is a hack, but fast for large ranges"
			"XOR"
			"Exchange bytes 0 and 1"
			"Exchange bytes 2 and 3"
	| hack blt |
	hack := Form new hackBits: self.	"The implementation is a hack, but fast for large ranges"
	blt := (BitBlt toForm: hack) sourceForm: hack.
	blt combinationRule: Form reverse.	"XOR"
		
	blt
		sourceY: 0;
		destY: 0;
		height: hack height;
		width: 1.
	#((0 1) (1 0) (0 1) (2 3)(3 2)(2 3))
		do: [:array | [:a :b| blt sourceX: a; destX: b; copyBits ] valueWithArguments: array ].
