variable accessing
removeInstanceVariable: aString 
	self privateInstanceVariableNames remove: aString.
	model removeInstanceVariable: aString from: self