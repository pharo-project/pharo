comparing
hash
	"Answer an integer value that is related to the identity of the receiver."

	^ color hash
		bitXor:
			(modified hash
				bitXor:
					(created hash
						bitXor:
							(description hash
								bitXor: (boolean hash bitXor: (float hash bitXor: (bytes hash bitXor: (integer hash bitXor: tags hash)))))))