accessing selection
shrinkSelection: aCharacterBlock
	"It shrinks existing selection to the left or to the right according to aCharacterBlock position."
	| newPointBlock newMarkBlock |
	newPointBlock := self pointBlock.
	newMarkBlock := self markBlock.
	(aCharacterBlock distance: newPointBlock) < (aCharacterBlock distance: newMarkBlock)
		ifTrue: [ newPointBlock := aCharacterBlock ]
		ifFalse: [ newMarkBlock := aCharacterBlock ].
	self
		markBlock: newMarkBlock
		pointBlock: newPointBlock