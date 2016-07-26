maintime
middleOf: aDuration
	"Return a Timespan where the receiver is the middle of the Duration"

	| duration |
	duration := aDuration asDuration.
	^ Timespan starting: (self - (duration / 2)) duration: duration.
		