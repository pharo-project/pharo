testing
isPureTranslation
	"Return true if the receiver specifies no rotation or scaling."

	^ angle = 0.0 and: [scale = 1.0]
