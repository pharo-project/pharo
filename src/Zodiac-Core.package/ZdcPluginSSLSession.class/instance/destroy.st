initialize
destroy
	"Destroys the platform handle in the VM plugin"

	handle isNil ifTrue: [ ^ self ].  
	self primitiveSSLDestroy: handle.
	handle := nil