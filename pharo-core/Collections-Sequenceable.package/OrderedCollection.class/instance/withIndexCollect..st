enumerating
withIndexCollect: elementAndIndexBlock 
	"Just like with:collect: except that the iteration index supplies the second argument to the block. Override superclass in order to use addLast:, not at:put:."

	| newCollection |
	newCollection := self species new: self size.
	firstIndex to: lastIndex do:
		[:index |
		newCollection addLast: (elementAndIndexBlock
			value: (array at: index)
			value: index - firstIndex + 1)].
	^ newCollection