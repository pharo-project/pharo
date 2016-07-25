obsolete
indicateDst
	"Change the indicators of the joins to the dst side."

	self joinMappings do: [:section |
		section selectionState: #dst].
	self
		changed;
		changed: #selectedDifferences