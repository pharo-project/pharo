accessing
upToEnd

	
	^String streamContents: [:strm | | ln |
		[(ln := self nextLine) isNil] whileFalse: [ 
			strm nextPutAll: ln; cr]]