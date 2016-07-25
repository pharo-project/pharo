filenames
logFilename
	"Answers the log filename in which we redirect stdout and stderr in both, the compilation and execution of the C program"
	^  self basicGeneralFilename, '.log'