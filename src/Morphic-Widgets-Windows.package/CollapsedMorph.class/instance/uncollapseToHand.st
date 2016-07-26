collapse/expand
uncollapseToHand
	"Hand the uncollapsedMorph to the user, placing it in her hand, after remembering appropriate state for possible future use"

	| nakedMorph |
	nakedMorph := uncollapsedMorph.
	uncollapsedMorph := nil.
	nakedMorph setProperty: #collapsedPosition toValue: self position.
	mustNotClose := false.  "so the delete will succeed"
	self delete.
	ActiveHand attachMorph: nakedMorph