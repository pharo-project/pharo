menu
addTargetingMenuItems: aCustomMenu hand: aHandMorph 
	"Add targeting menu items"
	aCustomMenu addLine.
	aCustomMenu add: 'set target' selector: #targetWith:.
	target
		ifNotNil: [aCustomMenu add: 'clear target' translated selector: #clearTarget]