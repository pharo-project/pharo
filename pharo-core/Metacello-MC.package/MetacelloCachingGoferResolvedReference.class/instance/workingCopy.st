querying
workingCopy
	"Answer a working copy or throw an error if not present."

	| pName |
	cachedVersion == nil ifTrue: [ ^super workingCopy ].
	pName := cachedVersion package name.
	^MCWorkingCopy allManagers
		detect: [ :each | pName = each packageName ]
		ifNone: [ self error: 'Working copy for ' , self name , ' not found' ]