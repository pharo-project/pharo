variable accessing
addInstanceVariable: aString 
	self privateInstanceVariableNames add: aString.
	model addInstanceVariable: aString to: self