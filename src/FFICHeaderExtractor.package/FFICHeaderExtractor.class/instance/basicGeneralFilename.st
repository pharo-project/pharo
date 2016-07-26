filenames
basicGeneralFilename
	"For internal usage only. It answers the base filname from which we then base the C program filename, the binary, the log,  the output, etc"
	^ self sharedPool name , '.', self platformName 