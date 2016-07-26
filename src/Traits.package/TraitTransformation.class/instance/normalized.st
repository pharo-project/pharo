accessing
normalized
	^self isEmpty
		ifFalse: [
			self subject: self subject normalized.
			self]
		ifTrue: [self subject normalized]
		
		