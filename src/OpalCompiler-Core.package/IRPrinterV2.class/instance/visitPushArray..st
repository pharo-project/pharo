visiting
visitPushArray: array
	
	array cons 
		ifTrue: [  
			stream nextPutAll: 'pushConsArray: ' ]
		ifFalse: [
			stream nextPutAll: 'pushNewArray: ' ].
	array size printOn: stream. 
