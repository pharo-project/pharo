private
pageHeight
	| howManyLines visibleHeight totalHeight ratio |
	howManyLines := self paragraph numberOfLines.
	visibleHeight := self visibleHeight.
	totalHeight := self totalTextHeight.
	ratio := visibleHeight / totalHeight.
	^(ratio * howManyLines) rounded - 2