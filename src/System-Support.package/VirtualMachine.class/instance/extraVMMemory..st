attributes
extraVMMemory: extraBytesToReserve
	"Request that the given amount of extra memory be reserved for use by the virtual machine to leave extra C heap space available for things like plugins, network and file buffers, and so on. This request is stored when the image is saved and honored when the image is next started up. Answer the previous value of this parameter."

	extraBytesToReserve < 0
		ifTrue: [self error: 'VM memory reservation must be non-negative'].
	^ self parameterAt: 23 put: extraBytesToReserve
