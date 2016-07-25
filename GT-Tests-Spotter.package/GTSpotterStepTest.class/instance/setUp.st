running
"protocol: running"
setUp
	super setUp.
	
	spotter := GTMockSpotter new.
	
	" having an assertion within the setup is ugly, but this code has to be performed before each test begins - so we do it here instead of copy/paste-ing this line to all tests - at least it never gets forgotten when new tests are defined "
	self shouldnt: [ spotter setText: '' from: self ] raise: Error. 
	
	currentStep := currentProcessor := currentCandidate := nil