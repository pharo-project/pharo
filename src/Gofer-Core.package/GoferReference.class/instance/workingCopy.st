querying
workingCopy
	"Answer a working copy or throw an error if not present."

	^ self workingCopyIfAbsent: [ self error: 'Working copy for ' , self name , ' not found' ]