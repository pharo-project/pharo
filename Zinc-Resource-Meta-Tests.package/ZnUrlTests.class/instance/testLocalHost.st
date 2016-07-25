testing
testLocalHost
	self assert: 'http://localhost:8080/foo' asZnUrl isLocalHost.
	self assert: 'http://127.0.0.1:8080/foo' asZnUrl isLocalHost.
	self assert: 'http://LOCALHOST:8080/foo' asZnUrl isLocalHost.
	self deny: 'http://zn.stx.eu' asZnUrl isLocalHost.
	self deny: '' asZnUrl isLocalHost.