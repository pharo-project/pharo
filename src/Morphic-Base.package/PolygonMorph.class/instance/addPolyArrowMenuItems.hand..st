menu
addPolyArrowMenuItems: aMenu hand: aHandMorph
	aMenu addLine.
	aMenu addWithLabel: '---' enablement: [ self isOpen and: [ arrows ~~ #none ] ] action: #makeNoArrows.
	aMenu addWithLabel: '-->' enablement: [ self isOpen and: [ arrows ~~ #forward ] ] action: #makeForwardArrow.
	aMenu addWithLabel: '<--' enablement: [ self isOpen and: [ arrows ~~ #back ] ] action: #makeBackArrow.
	aMenu addWithLabel: '<->' enablement: [ self isOpen and: [ arrows ~~ #both ] ] action: #makeBothArrows.
	aMenu add: 'customize arrows' selector: #customizeArrows:.
	(self hasProperty: #arrowSpec)
		ifTrue: [ aMenu add: 'standard arrows' selector: #standardArrows ]