scroll timing
resetTimer
	timeOfMouseDown := Time millisecondClockValue.
	timeOfLastScroll := timeOfMouseDown - 1000 max: 0.
	nextPageDirection := nil.
	currentScrollDelay := nil