helper
aRandomSelectionOfCharactersDo: aBlock
	self aRandomSelectionOfCodePointsDo: [ :cp | aBlock value: (Character codePoint: cp) ]