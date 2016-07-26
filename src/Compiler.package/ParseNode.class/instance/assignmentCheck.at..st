testing
assignmentCheck: encoder at: location
	"For messageNodes masquerading as variables for the debugger.
	For now we let this through - ie we allow stores ev
	into args.  Should check against numArgs, though."
	^ -1