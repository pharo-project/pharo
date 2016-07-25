helpers
ppidOf: childPid
	"Answers the parent pid (ppid) of the childPid
	"
	| tmpFileName openFilesNumber |
	tmpFileName := (FileSystem workingDirectory / ' ppidof-' , UUID new printString) fullName.
	tmpFileName asFileReference ensureDelete.
	Transcript show: tmpFileName; cr.
	self systemAccessor system: ('ps -p ', childPid , ' -o ppid=').
	openFilesNumber := tmpFileName asFileReference readStreamDo: [ :str |
		"The -1 is because the above command brings one line which is a header." 
		str contents trimmed asNumber
	].
	tmpFileName asFileReference ensureDelete.
	^ openFilesNumber
	