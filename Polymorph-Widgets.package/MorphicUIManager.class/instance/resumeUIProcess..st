ui process
resumeUIProcess: aProcess
	"Adopt aProcess as the project process -- probably because of proceeding from a debugger"
			
	UIProcess := aProcess.
	UIProcess resume