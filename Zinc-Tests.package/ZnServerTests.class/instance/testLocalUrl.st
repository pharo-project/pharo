testing
testLocalUrl
	self withServerDo: [ :server | 
		self assert: server localUrl scheme equals: #http.
		self assert: server localUrl host equals: NetNameResolver loopBackName.
		self assert: server localUrl port equals: server port ]