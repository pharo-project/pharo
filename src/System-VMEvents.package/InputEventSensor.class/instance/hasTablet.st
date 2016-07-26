tablet
hasTablet
	"Answer true if there is a pen tablet available on this computer."

	^ (self primTabletGetParameters: 1) notNil
