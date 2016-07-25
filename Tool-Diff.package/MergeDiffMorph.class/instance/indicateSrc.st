obsolete
indicateSrc
	"Change the indicators of the joins to the src side."

	self joinMappings do: [:section |
		section selectionState: #src].
	self
		changed;
		changed: #selectedDifferences