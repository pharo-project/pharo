initialization
removeTestPackage

	| matchText  |
	testPackage isEmpty
		ifTrue: [
			removeTestPackageModel label: 'Add All Packages Tests'.
			matchText := '*Test*'.
			testPackage :=MCWorkingCopy allManagers select: [ :package | matchText match:  package packageName ].
			selectedModel items: (self sort:(selectedModel listItems \ testPackage)).
			toSelectModel items: (self sort:(toSelectModel  listItems \ testPackage)) ]
		ifFalse: [
			removeTestPackageModel label: 'Remove All Packages Tests'.		
			toSelectModel items: (self sort: (toSelectModel listItems asOrderedCollection addAll: testPackage; yourself)).
			testPackage := OrderedCollection new ]. 
	toSelectModel updateList.
	selectedModel updateList.  	