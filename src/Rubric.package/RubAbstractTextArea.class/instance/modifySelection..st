accessing selection
modifySelection: aCharacterBlock
	"the aCharacterBlock becomes always the new point block. Whether the old point block or
	the old mark block becomes the new mark block, depends on the distance. If the new character block
	is near the old point block, we leave the old mark block unchaned."

	(self pointBlock distance: aCharacterBlock) < (self markBlock distance: aCharacterBlock)
		ifTrue: [ self pointBlock: aCharacterBlock ]
		ifFalse: [ self markBlock: self pointBlock pointBlock: aCharacterBlock ]