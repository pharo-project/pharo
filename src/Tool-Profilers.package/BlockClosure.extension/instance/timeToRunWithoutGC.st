*Tool-Profilers
timeToRunWithoutGC
	"Answer the number of milliseconds taken to execute this block without GC time."
 
	^ Smalltalk vm totalGCTime + self timeToRun asMilliSeconds -  Smalltalk vm totalGCTime
