accessing
boundsForBalloon

	"some morphs have bounds that are way too big"
	container ifNil: [^super boundsForBalloon].
	^self boundsInWorld intersect: container boundsInWorld ifNone: [ self boundsInWorld ]