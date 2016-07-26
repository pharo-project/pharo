next
testNextWithAnUnknownCharacterGetError
	| source scanner scannedToken |
	
	source := '¿'.
	scanner := self buildScannerForText: source.
	scannedToken := scanner next.
	self verifyErrorToken: scannedToken message: 'Unknown character' translated valueExpected: source