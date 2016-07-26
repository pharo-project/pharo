transactions
transact: aBlock
	"On GemStone, we want to optionally abort before command execution and commit after 
	 common execution. Other plaforms don't need to do anything special.
	 Returning out of block, skips commit."
	
	aBlock value