asserting
assertColorrefFor: aColor equals: anInteger
	| colorref |
	colorref := aColor asColorref.
	self
		assert: colorref = anInteger
		description:
			('(<1p>) asColorref was <2s>; should have been <3s>' expandMacrosWith: aColor with: colorref hex with: anInteger hex)