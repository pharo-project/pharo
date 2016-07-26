options
authenticator
	"Return the optional authenticator,
	the object that will be sent #authenticateRequest:do:
	to authenticate or refuse the requests. 
	When authentication succeeds, the block should be executed,
	when authentication fails, a appropriate response should be returned.
	If there is no authenticator, all requests will pass"
	
	^ self optionAt: #authenticator ifAbsent: [ nil ]