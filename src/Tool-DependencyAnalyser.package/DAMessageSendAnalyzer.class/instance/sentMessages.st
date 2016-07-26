computing
sentMessages
	^ self rPackage methods 
		flatCollect: #messages
		as: Set