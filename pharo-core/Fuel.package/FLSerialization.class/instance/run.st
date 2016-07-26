serializing
run
	"Serialize the graph starting at the root object."

	self analysisStep.
	self headerStep.
	self instancesStep.
	self referencesStep.
	self trailerStep.