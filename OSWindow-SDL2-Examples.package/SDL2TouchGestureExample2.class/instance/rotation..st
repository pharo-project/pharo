manage rotation
rotation: anEvent
	circleCenter := circleCenter 
		rotateBy: 2 * anEvent dTheta negated
		about: self windowCenter