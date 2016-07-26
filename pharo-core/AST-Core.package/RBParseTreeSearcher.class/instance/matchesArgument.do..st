searching
matchesArgument: aString do: aBlock 
	self addArgumentRule: (RBSearchRule searchFor: aString thenDo: aBlock)