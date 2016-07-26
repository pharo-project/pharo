history
next
	self browsingHistory pauseDuring: [
		self adopt: self browsingHistory next.
		self historyChanged ].