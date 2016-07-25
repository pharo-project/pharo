testing
isExpired
	^ (self expiresTimeStamp asUTC < DateAndTime now asUTC) or: [ self expires = 0 ]