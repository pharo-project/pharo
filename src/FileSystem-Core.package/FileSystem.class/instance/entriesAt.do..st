public-enumerating
entriesAt: aResolvable do: aBlock
	^ self 
		entriesAt: aResolvable 
		ifAbsent: [ store signalDirectoryDoesNotExist: ( self resolve: aResolvable) ]
		do: aBlock 