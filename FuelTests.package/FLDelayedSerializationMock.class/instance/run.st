serializing
run
	"Serialize the graph starting at the root object."

	self analysisStep.
	self headerStep.
	self instancesStep.
	(Delay forMilliseconds: 100) wait.
	self referencesStep.
	self trailerStep.