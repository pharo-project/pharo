private-checking
checkForMissingClasses: aCompiledMethod
	[ aCompiledMethod gtExamples ]
		on: GTExampleDeclaredClassMissing
		do: [ :exception | 
			self notifyForMissingClasses: exception ]