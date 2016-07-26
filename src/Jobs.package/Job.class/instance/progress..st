progress
progress: aNormalizedFloat
	"Set the progress: 0.0 - 1.0"

	currentValue := (min + ((max - min) * aNormalizedFloat)).
	self announceChange.