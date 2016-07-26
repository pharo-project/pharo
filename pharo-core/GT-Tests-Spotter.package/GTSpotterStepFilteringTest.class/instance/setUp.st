running
"protocol: running"
setUp
	super setUp.
	
	"Reser the state of spotter as the filter nees to be set before running a query."
	spotter := GTMockSpotter new.
	currentStep := currentProcessor := currentCandidate := nil