accessing selection
modifySelection: aCharacterBlock
	"It decides if the selection should be extended or shrinked."
	| prevPointBlock prevMarkBlock |
	prevPointBlock := self pointBlock.
	prevMarkBlock := self markBlock.
	(aCharacterBlock < prevPointBlock or: [ aCharacterBlock > prevMarkBlock ])
		ifTrue: [ self extendSelection: aCharacterBlock ]
		ifFalse: [ self shrinkSelection: aCharacterBlock ].