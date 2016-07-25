variable accessing
removeClassVariable: aString 
	self privateClassVariableNames remove: aString asSymbol.
	model removeClassVariable: aString from: self