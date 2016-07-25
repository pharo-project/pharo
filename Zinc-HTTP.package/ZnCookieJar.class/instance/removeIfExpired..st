operations
removeIfExpired: aCookie
	^ aCookie isExpired 
		ifTrue: [ 
			self remove: aCookie. 
			true ]
		ifFalse: [
			false ]