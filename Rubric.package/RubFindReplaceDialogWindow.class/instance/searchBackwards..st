accessing
searchBackwards: aBoolean
	self searchBackwards = aBoolean
		ifFalse: [ 
			self model searchBackwards: aBoolean.
			self findPolicyChanged ]