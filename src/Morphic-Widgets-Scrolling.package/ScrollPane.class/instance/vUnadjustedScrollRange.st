geometry
vUnadjustedScrollRange
	"Return the height extent of the receiver's submorphs."
	| submorphBounds |
	submorphBounds := scroller localSubmorphBounds ifNil: [^ 0].
	^ submorphBounds bottom
