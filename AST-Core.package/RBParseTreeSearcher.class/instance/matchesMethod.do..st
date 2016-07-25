searching
matchesMethod: aString do: aBlock 
	self addRule: (RBSearchRule searchForMethod: aString thenDo: aBlock)