halo commands
organizeIntoRow
	"Place my objects in a row-enforcing container"

	((AlignmentMorph inARow: (selectedItems asSortedCollection: [:x :y | x left < y left])) setNameTo: 'Row'; color: Color orange muchLighter; enableDragNDrop: true; yourself) openInHand
