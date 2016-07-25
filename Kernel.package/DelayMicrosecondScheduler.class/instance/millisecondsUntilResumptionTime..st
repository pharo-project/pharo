printing
millisecondsUntilResumptionTime: microsecondResumptionTime
	^ (microsecondResumptionTime - Time microsecondClockValue) // 1000