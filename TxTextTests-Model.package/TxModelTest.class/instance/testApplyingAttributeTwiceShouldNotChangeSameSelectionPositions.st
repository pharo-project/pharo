test-attribute
testApplyingAttributeTwiceShouldNotChangeSameSelectionPositions
	| pos1 pos2 |
	self newText: '123456'.
	selection all.
	selection start moveRight: 2.
	selection end moveLeft.
	
	
	pos1 := selection start asTextIndex.
	pos2 := selection end asTextIndex.
	
	selection applyAttribute:  TxFontAttribute new.
	selection applyAttribute:  TxFontAttribute new.
	
	self assert: selection start asTextIndex = pos1.
	self assert: selection end asTextIndex = pos2.
	
