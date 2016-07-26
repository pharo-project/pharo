class initialization
installMouseDecodeTable
	"Create a decode table that swaps the lowest-order 2 bits"
	ButtonDecodeTable := self swapMouseButtons
				ifTrue: [ByteArray withAll:
							((0 to: 255) collect: [:i |
								((i bitAnd: 1) << 1
									bitOr: (i bitAnd: 2) >> 1)
										bitOr: (i bitAnd: 16rFC) ])]
				ifFalse: [ByteArray withAll: (0 to: 255)]