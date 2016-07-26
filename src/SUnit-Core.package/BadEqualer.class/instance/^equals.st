comparing
= other 
	self class = other class
		ifFalse: [^ false].
	^ 100 atRandom < 30 