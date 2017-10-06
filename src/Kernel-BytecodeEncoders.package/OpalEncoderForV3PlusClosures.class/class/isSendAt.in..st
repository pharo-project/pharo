instruction stream support
isSendAt: pc in: method
	"Answer whether the bytecode at pc is a message-send."
	
	| byte |
	byte := method at: pc.
	^byte >= 131
	  and: [ "special send or short send" 
		byte >= 176 or: [ "long sends" byte <= 134]]