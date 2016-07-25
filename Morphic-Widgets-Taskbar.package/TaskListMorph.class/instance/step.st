stepping
step
	"Check the sensor for the command key to see if we're done."

	(self keepOpen not and: [self activeHand anyModifierKeyPressed not])
		ifTrue: [self done]