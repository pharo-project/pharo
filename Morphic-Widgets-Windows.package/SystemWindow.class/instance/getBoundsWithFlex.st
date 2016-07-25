resize/collapse
getBoundsWithFlex
	"Return the lastest bounds rectangle with origin forced to global coordinates"

	self isFlexed
		ifTrue: [^ ((owner transform localPointToGlobal: bounds topLeft)
										extent: bounds extent)]
		ifFalse: [^ self bounds].
