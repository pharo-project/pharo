stored blocks
storedBlockSize
	"Compute the length for the current block when stored as is"
	^3 "block type bits" 
		+ (8 - (encoder bitPosition + 3 bitAnd: 7) bitAnd: 7)"skipped bits to byte boundary"
			+ 32 "byte length + chksum" 
				+ (blockPosition - blockStart * 8) "actual data bits".