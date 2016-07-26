initialization
initialize
	super initialize.
	maximumSize := self class defaultMaximumSize.
	fontTable := self dictionaryClass new: 100.
	used := 0.
	fifo := self fifoClass new
	