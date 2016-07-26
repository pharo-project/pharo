halo commands
organizeIntoColumn
	"Place my objects in a column-enforcing container"

	((AlignmentMorph inAColumn: (selectedItems asSortedCollection: [:x :y | x top < y top])) setNameTo: 'Column'; color: Color orange muchLighter; enableDragNDrop: true; yourself) openInHand
