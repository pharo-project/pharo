history behavior
historyChanged
	self setWindowTitle.
	self changed: #getHistoryList.
	self changed: #currentHistoryIndex