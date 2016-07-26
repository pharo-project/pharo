enumerating
reverseWithIndexDo: elementAndIndexBlock 
	"Just like reverseWith:do: except that the iteration index supplies the second argument to the block."
	self size to: 1 by: -1 do: [:index |
		elementAndIndexBlock
			value: (self at: index)
			value: index]