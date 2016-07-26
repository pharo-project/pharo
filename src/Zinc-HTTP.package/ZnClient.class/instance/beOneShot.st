options
beOneShot
	"Switch me to use connections only for a single request/response cycle.
	By default, I will keep HTTP connections open according to the HTTP 1.1 spec.
	When oneShot is true, I will add a Connection: close header to requests and close after one cycle.
	I cannot be one shot when #streaming is true because the stream would be closed too soon."
	
	self streaming
		ifTrue: [ self error: 'I cannot be one shot when #streaming is true.' ].
	self optionAt: #oneShot put: true   