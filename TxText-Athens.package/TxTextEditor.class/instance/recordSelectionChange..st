private
recordSelectionChange: aBlock

	| operation announcement |
	operation := (TxSelectionChangeOperation editor: self).
	operation captureDuring: aBlock.
	self addUndo: operation. 
	
	announcement := TxSelectionChangedAnnouncement new
		editor: self;
		newSelection: selection;
		yourself.
	self announce: (announcement).
	