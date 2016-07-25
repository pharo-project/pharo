testing
testNonAscii
	| encoder |
	encoder := ZnPercentEncoder new.
	self 
		assert: encoder characterEncoder 
		equals: (ZnCharacterEncoder newForEncoding: 'utf-8').
	self 
		assert: (encoder encode: 'élève en Français') 
		equals: '%C3%A9l%C3%A8ve%20en%20Fran%C3%A7ais'.
	self 
		assert: (encoder decode: '%C3%A9l%C3%A8ve%20en%20Fran%C3%A7ais') 
		equals: 'élève en Français'