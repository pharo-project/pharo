querying
lineIndexForPoint: aPoint
	"Answer the index of the line in which to select the character nearest to aPoint."
	| i py |
	py := aPoint y truncated.

	"Find the first line at this y-value"
	i := (self fastFindFirstLineSuchThat: [:line | line bottom > py]) min: self lines size.

	"Now find the first line at this x-value"
	[i < self lines size and: [(self lines at: i+1) top = (self lines at: i) top
				and: [aPoint x >= (self lines at: i+1) left]]]
		whileTrue: [i := i + 1].
	^ i