geometry
hUnadjustedScrollRange
	"Return the width extent of the receiver's submorphs."

	| submorphBounds |
	submorphBounds := scroller localSubmorphBounds ifNil: [^ 0].
	^ submorphBounds right
