methods
endTrueFalse
	self isVariable 
		ifTrue: 
			[self printString.
			self isVariable printString]
		ifFalse: 
			[self printString.
			^4]