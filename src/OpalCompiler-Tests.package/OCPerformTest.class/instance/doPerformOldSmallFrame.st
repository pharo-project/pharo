helper methods
doPerformOldSmallFrame
	"The perform primitive reuses the current stack frame.
	When this was small, it would cause the perform primitive to fail, when the amount of arguments were too high"
	
	^self perform: #a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:a11:a12:a13:a14:a15:
		withArguments: (Array new: 15).

