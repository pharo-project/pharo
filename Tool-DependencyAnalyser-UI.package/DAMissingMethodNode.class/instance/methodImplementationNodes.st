api
methodImplementationNodes
	^ (self content implementors collect: 
		[ :method | 
		DAMethodImplementationNode new
			content: method;
			parentNode: self;
			yourself ])
		sorted: [ :a :b | a content package name < b content package name ]