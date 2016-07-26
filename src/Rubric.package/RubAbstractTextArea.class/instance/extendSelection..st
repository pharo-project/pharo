accessing selection
extendSelection: aCharacterBlock
	"It extends existing selection to the left or to the right according to aCharacterBlock position."
	| prevMarkBlock prevPointBlock newPointBlock newMarkBlock |
	prevMarkBlock := self markBlock.
	prevPointBlock := self pointBlock.
	newMarkBlock := (prevMarkBlock max: prevPointBlock) max: aCharacterBlock.
	newPointBlock := (prevMarkBlock min: prevPointBlock) min: aCharacterBlock.
	self
		markBlock: newMarkBlock
		pointBlock: newPointBlock