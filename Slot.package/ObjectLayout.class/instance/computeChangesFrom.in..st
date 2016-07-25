diff
computeChangesFrom: other in: modification
	other hasSlots ifTrue: [
		modification removals: other allSlots asArray ]