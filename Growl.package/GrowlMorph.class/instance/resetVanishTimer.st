internal
resetVanishTimer

	vanishTime := DateAndTime now + self vanishDelay.
	self resetAlpha.