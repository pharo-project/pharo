menu
addPolyShapingMenuItems: aMenu hand: aHandMorph
	aMenu addLine.
	aMenu addWithLabel: 'make inscribed diamondOval' enablement: [ self isClosed ] action: #diamondOval.
	aMenu addWithLabel: 'make enclosing rectangleOval' enablement: [ self isClosed ] action: #rectangleOval