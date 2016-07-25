actions
run
	environment classesAndTraits
		do: [ :class |
			self checkClass: class.
			self checkMethodsForClass: class.
			
			self checkClass: class classSide.
			self checkMethodsForClass: class classSide ]
		displayingProgress: [ :class | 'Running critics on: ', class name ].
	environment packages 
		do: [ :package | self checkPackage: package ]
		displayingProgress: [ :package | 'Running critics on: ', package name ].