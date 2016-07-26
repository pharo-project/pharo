evaluating
valueNoContextSwitch
	"An exact copy of BlockClosure>>value except that this version will not preempt
	 the current process on block activation if a higher-priority process is runnable.
	 Primitive. Essential."
	<primitive: 221>
	numArgs ~= 0 ifTrue:
		[self numArgsError: 0].
	self primitiveFailed