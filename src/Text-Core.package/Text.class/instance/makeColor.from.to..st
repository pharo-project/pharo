emphasis
makeColor: color from: start to: stop
	| attribute |
	attribute := TextColor color: color.

	^ self addAttribute: attribute from: start to: stop