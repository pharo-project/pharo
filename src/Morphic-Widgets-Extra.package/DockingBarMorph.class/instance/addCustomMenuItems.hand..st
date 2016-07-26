menu
addCustomMenuItems: aMenu hand: aHandMorph 
	"Populate aMenu with appropriate menu items for a  
	yellow-button (context menu) click."
	super addCustomMenuItems: aMenu hand: aHandMorph.

	aMenu addLine.
	aMenu addUpdating: #autoGradientString selector: #toggleAutoGradient.
	self isFloating
		ifFalse: [
			aMenu addUpdating: #fillsOwnerString selector: #toggleFillsOwner.
			aMenu addUpdating: #avoidVisibleBordersAtEdgeString selector: #toggleAvoidVisibleBordersAtEdge]