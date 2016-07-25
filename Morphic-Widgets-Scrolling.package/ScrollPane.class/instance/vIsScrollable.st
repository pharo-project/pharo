geometry testing
vIsScrollable
"Return whether the verticle scrollbar is scrollable"

	"If the contents of the pane are too small to scroll, return false."
	^ self vLeftoverScrollRange > 0
		"treat a single line as non-scrollable"
		and: [self vTotalScrollRange > (self scrollDeltaHeight * 3/2)]
