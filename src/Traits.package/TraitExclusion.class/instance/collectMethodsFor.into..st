enquiries
collectMethodsFor: aSelector into: methodDescription
	(self exclusions includes: aSelector) ifFalse: [
		self subject
			collectMethodsFor: aSelector
			into: methodDescription]