metacello
workingCopies
	^ MCWorkingCopy allManagers asSortedCollection: [ :a :b | a name <= b name]