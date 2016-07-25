installing
installScript: aString
	| sel pi |
	sel := (self scriptSelector, ':') asSymbol.
	pi := self packageSet.
	(pi respondsTo: sel)
		ifTrue: [pi perform: sel with: aString]