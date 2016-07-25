morphic
update
	self changed: #argumentList; changed: #argumentIndex.
	self changed: #isUpEnabled; changed: #isDownEnabled; changed: #isOkEnabled.
	labelMorph contents: self methodName printString