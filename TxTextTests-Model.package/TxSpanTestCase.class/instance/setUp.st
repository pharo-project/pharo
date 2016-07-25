running
setUp
	super setUp.
	textModel := TxModel new.
	
	span := self spanClass new.
	span text: textModel