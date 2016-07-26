debugging actions
clear
	"If after resuming the process the user does plan to reuse this session with
	the same process, it should call this method."
	interruptedProcess := nil.
	self updateContextTo: nil