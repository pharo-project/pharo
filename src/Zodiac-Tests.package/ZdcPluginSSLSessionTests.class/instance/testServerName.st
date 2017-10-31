tests
testServerName
	
	[ session serverName; serverName: nil ] on: PrimitiveFailed do: [ ^ self skip ].
	
	self assert: session serverName equals: nil.
	
	session serverName: 'pharo.org'.
	
	self assert: session serverName equals: 'pharo.org'.
	
	session serverName: 'smalltalkhub.com'.
	
	self assert: session serverName equals: 'smalltalkhub.com'.
	
	session serverName: nil.
	
	self assert: session serverName equals: nil.
