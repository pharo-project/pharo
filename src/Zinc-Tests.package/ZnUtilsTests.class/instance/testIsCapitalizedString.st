testing
testIsCapitalizedString
	self assert: (ZnUtils isCapitalizedString: 'Content-Type').
	self assert: (ZnUtils isCapitalizedString: 'CONTENT-type') not.
	self assert: (ZnUtils isCapitalizedString: 'content-TYPE') not.
	self assert: (ZnUtils isCapitalizedString: 'WWW-Authenticate') not.
	self assert: (ZnUtils isCapitalizedString: 'Connection')