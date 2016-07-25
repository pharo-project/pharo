filtering
filteredStackOfSize: anInteger

	^ self debugger class filterCommonMessageSends 
		ifTrue: [
			self session 
				stackOfSize: anInteger 
				usingFilters: self enabledFilters ]
		ifFalse: [ 
			self session stackOfSize: anInteger ]
		
	 