refreshing
informRefreshError

	| message |
	message := self title asString, ': Refresh error'.
	message logCr.
	self inform: message

