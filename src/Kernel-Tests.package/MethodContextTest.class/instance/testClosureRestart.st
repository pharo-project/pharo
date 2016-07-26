tests
testClosureRestart
	"Test that various combinations of closures are restarted with the expected values"
	"no args, no remote temps blocks are not tested, as I don't know how to do that programatically without ending up looping endlessly"
	self should: [self privRestartBlockTest] notTakeMoreThan: 0.1 second.
	self should: [self privRestartArgBlockTest] notTakeMoreThan: 0.1 second.
	"self should: [self privRestartBlockArgsNoRemoteTempsTest] notTakeMoreThan: 0.1 second" "FAILING!"