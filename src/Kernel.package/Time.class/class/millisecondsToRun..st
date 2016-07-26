general inquiries
millisecondsToRun: timedBlock 
	"Answer the number of milliseconds timedBlock takes to return its value."

	^(self microsecondsToRun: timedBlock) // 1e3