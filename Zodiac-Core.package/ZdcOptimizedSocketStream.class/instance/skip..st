stream in
skip: count
	"Skip over count bytes. Overwritten, optimized"
	
	| skipCount |
	skipCount := 0.
	[ | leftToSkip skipping |
		leftToSkip := count - skipCount.
		skipping := readBuffer availableForReading min: leftToSkip.
		readBuffer advanceReadPointer: skipping.
	     (skipCount := skipCount + skipping) = count ] whileFalse: [ self fillReadBuffer ]