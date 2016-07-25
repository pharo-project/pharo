helpers
numberOfZombiesProcesses
	"This is estimation number. Not to be used for real code. IT's basically 
	to check the number of zombies at the beginning of a  test and at the end, to validate we are not leaving behind new zombies. 
	This should work in most Unix-like and Linux
	"
	| tmpFileName zombiesCount |
	tmpFileName := (FileSystem workingDirectory / 'zombiesByPharoVM-' , UUID new printString) fullName.
	tmpFileName asFileReference ensureDelete.
	self systemAccessor system: ('ps axo ppid=,stat= | grep ', OSSVMProcess vmProcess pid printString, ' | awk ''$2~/^Z/ { print $2 }'' | wc -l  > ', tmpFileName, ' 2>&1').
	zombiesCount := tmpFileName asFileReference readStreamDo: [ :str |
		str contents trimmed asNumber
	].
	tmpFileName asFileReference ensureDelete.
	^ zombiesCount
	