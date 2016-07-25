instruction stream support
isCreateBlockAt: pc in: method
	"Answer whether the bytecode at pc is a block creation bytecode."
	
	"250		11111010 	eeiiikkk		jjjjjjjj		Push Closure Num Copied iii (+ExtA//16*8) Num Args kkk (+ ExtA\\16*8) BlockSize jjjjjjjj (+ExtB*256). ee = num extensions"

	| byte |
	byte := self nonExtensionBytecodeAt: pc in: method.
	^ byte = 250