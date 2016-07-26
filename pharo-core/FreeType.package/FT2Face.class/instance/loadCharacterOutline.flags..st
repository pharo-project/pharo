outlines
loadCharacterOutline: index flags: flags
	| em outline |
	em := unitsPerEm.
	self setPixelWidth: em height: em.
	self loadCharacter: index flags: flags.
	outline := FT2Outline new.
	outline primLoadSizesFrom: self.
	outline allocateArrays.
	outline primLoadArraysFrom: self.

	^outline