instance creation
open: aSyntaxError 
	"Answer a standard system view whose model is an instance of me."
	<primitive: 19>
	"Simulation guard"
	| process |
	process := Processor activeProcess.
	UIManager default spawnNewProcessIfThisIsUI: process.
	UIManager default defer: [
		self buildMorphicViewOn: aSyntaxError.
	].
	^ process suspend