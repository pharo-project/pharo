open/close
open: fileName forWrite: writeMode 
	"Open the file with the given name. If writeMode is true, allow writing, otherwise open the file in read-only mode."
	| f |
	f := fileName asVmPathName.

	fileID := StandardFileStream retryWithGC:[self primOpen: f writable: writeMode] 
					until:[:id| id notNil] 
					forFileNamed: fileName.
	fileID ifNil: [^ nil].  "allows sender to detect failure"
	name := fileName.
	self register.
	rwmode := writeMode.
	buffer1 := String new: 1.
	self enableReadBuffering
	