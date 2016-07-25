evaluating
valueWithArguments: anArray

	^ receiver 
		perform: selector 
		withArguments: (self collectArguments: anArray)