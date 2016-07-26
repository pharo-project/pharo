halo commands
addCustomMenuItems: aMenu hand: aHandMorph
	"Add custom menu items to the menu"

	super addCustomMenuItems: aMenu hand: aHandMorph.
	aMenu addLine.
	aMenu add: 'add or remove items' target: self selector: #addOrRemoveItems: argument: aHandMorph.
	aMenu addList: {
		#-.
		{'Place into a row' . #organizeIntoRow}.
		{'Place into a column' . #organizeIntoColumn}.
		#-.
		{'Align left edges' . #alignLeftEdges}.
		{'Align top edges' . #alignTopEdges}.
		{'Align right edges' . #alignRightEdges}.
		{'Align bottom edges' . #alignBottomEdges}.
		#-.
		{'Align centers vertically' . #alignCentersVertically}.
		{'Align centers horizontally' . #alignCentersHorizontally}.
		}.

	self selectedItems size > 2
		ifTrue: [
			aMenu addList: {
				#-.
				{'Distribute vertically' . #distributeVertically}.
				{'Distribute horizontally' . #distributeHorizontally}.
				}.
		].
