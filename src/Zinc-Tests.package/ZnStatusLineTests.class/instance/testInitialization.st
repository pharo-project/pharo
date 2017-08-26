testing
testInitialization
	"Make sure everything is properly initialized"
	
	ZnConstants officialHTTPStatusCodesSpec pairsDo: [ :code :reason |
		| statusLine |
		statusLine := ZnStatusLine code: code.
		self assert: statusLine code equals: code.
		self assert: statusLine reason equals: reason ]