as yet unclassified
calculateMaxVelocity

	"max velocity must depend from the height of the band"
	^ (self band width / 15) max: self class defaultMaxVelocity