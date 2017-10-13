evaluating
flushChangesFile
	"Yes, we need to do this in order to flush the file, puaj"
	logSource ifTrue: [
		ChangesLog default logChange: '----End fileIn----'].