private-checking
checkForMissingClasses
	[ GTExampleFinder allGTExamples ]
		on: GTExampleDeclaredClassMissing
		do: [ :exception | 
			self notifyForMissingClasses: exception ]