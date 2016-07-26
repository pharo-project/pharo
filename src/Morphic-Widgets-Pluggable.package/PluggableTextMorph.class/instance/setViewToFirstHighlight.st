actions
setViewToFirstHighlight
	| min lines |
	
	min := (self highlights detectMin: [:e | e lineNumber ]) lineNumber.
	lines := self textMorph paragraph lines size.
	
	self vScrollValue: (min/lines)