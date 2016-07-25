private - 2 swapping
swapSourcePointerOfMethod: method

	remoteStringMap at: method ifPresent: [ :remoteString |
		method setSourcePointer: remoteString sourcePointer ]