changing process state
run
	"Suspend current process and execute self instead"

	| proc |
	proc := Processor activeProcess.
	[	proc suspend.
		self resume.
	] forkAt: Processor highestPriority