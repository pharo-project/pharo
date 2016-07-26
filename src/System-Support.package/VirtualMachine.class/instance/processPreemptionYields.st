testing
processPreemptionYields
	"Answer whether the VM causes a process to yield on process preemption, i.e. to put a preempted process at the back of its run queue.  If the parameter is unavailable (non-Cog VMs) or bit 2 (4) is 0 then preemption yields."

	^(([self parameterAt: 48]
			on: Error
			do: [:ex| ^true]) allMask: 2r100) not