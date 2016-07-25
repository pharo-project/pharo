reporting
reportVMOptions: aStream
	self header: 'Virtual Machine Commandline Options' on: aStream.
	self enumerate: [:idx | Smalltalk vm optionAt: idx] startAt: 1 on: aStream