halting
now: aString
	"set a breakpoint with some explanation"
	<debuggerCompleteToSender>
	self signal: aString