debug and other
resumeAfterStepError
	"Resume stepping after an error has occured."

	self startStepping. "Will #step"
	self removeProperty:#errorOnStep. "Will remove prop only if #step was okay"
