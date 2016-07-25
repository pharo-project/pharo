hands
is: hand overlappingDamagedAreaIn: damageList
	| handBounds |
	handBounds := hand fullBounds.
	^ damageList anySatisfy: [ :r | r intersects: handBounds ]