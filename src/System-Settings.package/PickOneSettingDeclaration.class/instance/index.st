user interface
index
	self realValue ifNil: [self realValue: self default].
	^ self domainValues indexOf: (self domainValues detect: [:dv | dv realValue = self realValue] ifNone: [^ 0])