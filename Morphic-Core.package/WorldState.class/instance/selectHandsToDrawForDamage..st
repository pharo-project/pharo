hands
selectHandsToDrawForDamage: damageList
	"Select the set of hands that must be redrawn because either (a) the hand itself has changed or (b) the hand intersects some damage rectangle."

	^ hands
		select: [ :hand | hand needsToBeDrawn and: [ hand hasChanged or: [ self is: hand overlappingDamagedAreaIn: damageList ] ] ]