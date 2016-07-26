queries
packages 
	^ (self classes 
		collect: #package 
		as: Set)
		asArray