private
grow 
	| newSelf |
	newSelf := self species newForCapacity: self basicSize * 2.
	1 to: self basicSize do:
		[:i | (self basicAt: i) ifNotNil: [ :key | newSelf at: key put: (array at: i)]].
	self becomeForward: newSelf