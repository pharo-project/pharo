visual properties
useSolidFill
	"Make receiver use a solid fill style (e.g., a simple color)"
	self fillStyle isSolidFill ifTrue:[^self]. "Already done"
	self fillStyle: self fillStyle asColor. "Try minimizing changes"