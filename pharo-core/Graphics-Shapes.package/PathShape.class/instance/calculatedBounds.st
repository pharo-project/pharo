compute
calculatedBounds
	"Answer the bounds of the receiver calculated from the
	receiver's vertices."

	|tl br|
	self vertices ifEmpty: [^nil].
	tl := br := self vertices first.
	self vertices allButFirstDo: [:v |
		tl := tl min: v.
		br := br max: v].
	^tl corner: br + 1