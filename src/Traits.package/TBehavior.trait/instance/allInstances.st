accessing instances and variables
allInstances
	"Answer all instances of the receiver."
	<primitive: 177>
	"The primitive can fail because memory is low.  If so, fall back on the old
	 enumeration code, which gives the system a chance to GC and/or grow.
	 Because aBlock might change the class of inst (for example, using become:),
	 it is essential to compute next before aBlock value: inst."
	| inst insts next |
	insts := WriteStream on: (Array new: 64).
	inst := self someInstance.
	[inst == nil] whileFalse:
		[next := inst nextInstance.
		 (inst == insts or: [inst == insts originalContents]) ifFalse: [insts nextPut: inst].
		 inst := next].
	^insts contents