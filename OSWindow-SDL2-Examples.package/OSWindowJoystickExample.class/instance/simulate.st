simulation
simulate
	| deltaTime |
	lastSimulationTime ifNil: [ lastSimulationTime := Time millisecondClockValue * 0.001].
	currentTime := Time millisecondClockValue * 0.001.
	deltaTime := currentTime - lastSimulationTime.
	lastSimulationTime := currentTime.
	
	self
		simulateCharacter: deltaTime;
		simulateBullets: deltaTime.