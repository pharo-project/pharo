history
previous
	self browsingHistory pauseDuring: [
		self adopt: self browsingHistory previous.
		self historyChanged ]