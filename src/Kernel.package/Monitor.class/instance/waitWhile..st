waiting-basic
waitWhile: aBlock
	"Conditional waiting for the default event.
	The current process gets blocked and leaves the monitor only if the argument block
	evaluates to true. This means that another process can enter the monitor. When the 
	default event is signaled, the original process is resumed, which means that the condition
	(argument block) is checked again. Only if it evaluates to false, does execution proceed.
	Otherwise, the process gets blocked and leaves the monitor again..."

	^ self waitWhile: aBlock for: nil