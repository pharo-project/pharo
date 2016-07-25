testing
canBeDisplay
	^ (self isNew or: [ self isRemoved ]) or: [ self hasChanged ].