instance creation
random
	"Return a random color that isn't too dark or under-saturated."

	^ self 
		h: (360.0 * RandomStream next)
		s: (0.3 + (RandomStream next * 0.7))
		v: (0.4 + (RandomStream next * 0.6))
		alpha: 1.0