options
authenticator: object
	"Set the object that will be sent #authenticateRequest:do:
	to authenticate or refuse the requests. Can be nil.
	When authentication succeeds, the block should be executed,
	when authentication fails, a appropriate response should be returned"

	self optionAt: #authenticator put: object