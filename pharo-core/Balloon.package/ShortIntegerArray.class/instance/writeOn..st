objects from disk
writeOn: aStream
	aStream nextInt32Put: self basicSize.
	1 to: self basicSize do: [ :i | 
		self write: (self basicAt: i) on: aStream ]