menu
addCustomMenuItems: aCustomMenu hand: aHandMorph

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	self addLabelItemsTo: aCustomMenu hand: aHandMorph.
	aCustomMenu add: 'change action selector' selector: #setActionSelector.
	aCustomMenu add: 'change arguments' selector: #setArguments.
	aCustomMenu add: 'change when to act' selector: #setActWhen.
	self addTargetingMenuItems: aCustomMenu hand: aHandMorph.
