initialization
initialize

	super initialize.
		
	nextUpdate := Time millisecondClockValue.
	updateDelay := 50. "milliseconds"
	updateConstraints := Set new.
	constraintsChecker := [ :constraints :aBlock | constraints allSatisfy: aBlock ].