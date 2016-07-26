instruction decoding
respondsToUnknownBytecode
	"This method is triggerred by the VM when the interpreter tries to execute an unknown bytecode"
	
	| unknownBytecode |
	unknownBytecode := self method at: self pc.
	self error: 'VM cannot run unknown bytecode ', unknownBytecode printString
	
