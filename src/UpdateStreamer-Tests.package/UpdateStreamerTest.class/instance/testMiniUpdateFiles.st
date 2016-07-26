tests
testMiniUpdateFiles
	self deny: (self miniUpdate includes: Character lf).
	self deny: (self miniUpdateLf includes: Character cr).