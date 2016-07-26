memory space
lowSpaceThreshold 
	"Return the low space threshold. When the amount of free memory (after garbage collection) falls below this limit, the system is in serious danger of completely exhausting memory and crashing. This limit should be made high enough to allow the user open a debugger to diagnose a problem or to save the image."

	^ 400000  "Enough for JIT compiler"