running
runClasses: aCollectionOfClasses named: aString
	| classEnvironment |
	classEnvironment := RBBrowserEnvironment new
		forClasses: aCollectionOfClasses.
	classEnvironment label: aString.
	^ self runEnvironment: classEnvironment