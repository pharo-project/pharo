visiting
visitPushArray: array

	array cons 
		ifTrue: [  
			gen pushConsArray: array size ]
		ifFalse: [
			gen pushNewArray: array size ]