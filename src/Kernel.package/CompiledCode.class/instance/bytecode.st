accessing
bytecode
	"Answer an ByteArray of the btyecode of the method."

	| start stop bytecode |

	start := self initialPC.
	stop := self endPC.
	
	bytecode := ByteArray new: (stop - start + 1).
	start to: stop do: [:index |
		bytecode byteAt: index - start + 1 put: (self byteAt: index)].
	^bytecode