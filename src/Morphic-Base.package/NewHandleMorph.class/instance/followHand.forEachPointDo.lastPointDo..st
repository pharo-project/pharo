all
followHand: aHand forEachPointDo: block1 lastPointDo: block2
	hand := aHand.
	pointBlock := block1.
	lastPointBlock := block2.
	self position: hand lastEvent cursorPoint - (self extent // 2)