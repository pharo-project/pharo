querying
implementors
	^ methods ifNil: [ 
		methods := (SystemNavigation default allImplementorsOf: self name) 
			collect: [ :each | each compiledMethod ] ]