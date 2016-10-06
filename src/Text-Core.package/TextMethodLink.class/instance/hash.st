comparing
hash
	^ (((super hash + selector hash) hashMultiply + browseSenders hash) hashMultiply
		+ self sourceNodeInterval hash) hashMultiply