variable accessing
addClassVariable: aString 
	self privateClassVariableNames add: aString asSymbol.
	model addClassVariable: aString to: self