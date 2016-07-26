container protocol
top
	"Note we should really check for contiguous pixels here"
	| outerWidth |
	outerWidth := minWidth + (2*OuterMargin).
	^ (self vertProfile findFirst: [:count | count >= outerWidth]) - 1
		+ shadowForm offset y