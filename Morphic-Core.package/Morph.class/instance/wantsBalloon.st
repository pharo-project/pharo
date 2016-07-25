halos and balloon help
wantsBalloon
	"Answer true if receiver wants to show a balloon help text is a few moments."

	^ (self balloonText notNil) and: [self balloonHelpEnabled]