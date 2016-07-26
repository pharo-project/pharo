initialize-release
interpretNextInstructionUsing: aScanner 
	
	bingo := false.
	aScanner interpretNextInstructionFor: self.
	^bingo