searching
matches: aString do: aBlock 
	self addRule: (RBSearchRule searchFor: aString thenDo: aBlock)