scrollpane-calculations
calculateMaxVelocity
	"max velocity must depend from the height of the band"
	^ (self band height / 15) max: self class defaultMaxVelocity