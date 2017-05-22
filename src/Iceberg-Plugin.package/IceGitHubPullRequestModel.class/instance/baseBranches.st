private
baseBranches
	^ baseBranches ifNil: [ 
		baseBranches := STON fromString: self requestBaseBranches ]