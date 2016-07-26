accessing
gradientRampForColor: c
	"Answer the background gradient ramp to use."
	
	^{0.0->c darker duller. 0.1-> c lighter.
		0.9->c twiceLighter. 1.0->c darker}