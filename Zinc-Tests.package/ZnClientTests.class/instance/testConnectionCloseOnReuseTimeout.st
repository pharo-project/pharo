testing
testConnectionCloseOnReuseTimeout
	| client firstSocket secondSocket|
	client := ZnClient new.
	client connectionReuseTimeout: 0.
	self withServerDo: [ :server | 
		client 
			url: server localUrl;
			get.
		firstSocket := client connection.
		client get.
		secondSocket := client connection.		
		self assert: (firstSocket = secondSocket) not.
		self deny: firstSocket isConnected.
		firstSocket close.
		client close ]