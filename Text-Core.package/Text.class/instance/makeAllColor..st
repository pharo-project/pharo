emphasis
makeAllColor: color
	| attribute |
	attribute := TextColor color: color.

	^ self addAttribute: attribute from: 1 to: self size