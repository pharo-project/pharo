halo commands
duplicate
	"Make a duplicate of the receiver and havbe the hand grab it"

	selectedItems :=  selectedItems collect: #duplicate.
	selectedItems reverseDo: [:m | (owner ifNil: [ActiveWorld]) addMorph: m].
	dupLoc := self position.
	ActiveHand grabMorph: self.
	