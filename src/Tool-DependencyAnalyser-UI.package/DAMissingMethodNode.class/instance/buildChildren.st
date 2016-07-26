api
buildChildren
	^ self messageSendNodes 
		addAll: self methodImplementationNodes;
		yourself