private
recordSelectionChange: aBlock

	| operation |
	operation := (TxSelectionChangeOperation editor: self).
	operation captureDuring: aBlock.
	self addUndo: operation. 
	
	self notifySelectionChange: selection