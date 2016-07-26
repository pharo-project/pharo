private
resumeProcessWithValue: aValue
	"Make sure the interrupted process is restored properly and restart the low space handler"
	
	self returnValue: aValue from: interruptedProcess suspendedContext.
	self resumeProcess.	