querying
workingCopyIfAbsent: aBlock
	"Answer a working copy or throw an error if not present."

	^ MCWorkingCopy allManagers
		detect: [ :each | self packageName = each packageName ]
		ifNone: aBlock