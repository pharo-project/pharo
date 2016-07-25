enumerating
withIndexCollect: elementAndIndexBlock 
	"Just like with:collect: except that the iteration index supplies the second argument to the block."
	| result |
	result := self species new: self size.
	1 to: self size do:
		[:index | result at: index put:
		(elementAndIndexBlock
			value: (self at: index)
			value: index)].
	^ result