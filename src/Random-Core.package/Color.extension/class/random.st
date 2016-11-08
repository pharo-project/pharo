*Random-Core
random
	"Return a random color that isn't too dark or under-saturated."

	^ self 
		h: (360.0 * SharedRandom globalGenerator next)
		s: (0.3 + (SharedRandom globalGenerator next * 0.7))
		v: (0.4 + (SharedRandom globalGenerator next * 0.6))
		alpha: 1.0