private
integerConstantArgument: anInteger
	^ FFIConst value: anInteger type: (self resolveExternalType:  #uint32)