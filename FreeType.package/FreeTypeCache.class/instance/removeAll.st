add-remove
removeAll
	fontTable := self dictionaryClass new: 100.
	fifo := self fifoClass new.
	used := 0.

	