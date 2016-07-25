private
withAttribute: att do: strmBlock
	| pos1 val |
	pos1 := self position.
	val := strmBlock value.
	collection addAttribute: att from: pos1+1 to: self position.
	^ val