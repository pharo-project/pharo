accessing
value: anObject in: aContext
	(self validate: anObject) ifFalse: [^ self].
	aContext addPort: self.
	self changeValueTo: anObject in: aContext.