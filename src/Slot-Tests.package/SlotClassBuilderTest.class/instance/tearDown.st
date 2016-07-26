running
tearDown
	"We remove the classes that could have been created during test run"
	super tearDown.

	SystemAnnouncer uniqueInstance suspendAllWhile: [
		{ self aClassName. self anotherClassName. self yetAnotherClassName. self yetYetAnotherClassName } do: [ :each | 
			Smalltalk globals 
				at: each 
				ifPresent: [ :class | class removeFromSystem ]]].

	SystemAnnouncer uniqueInstance unsubscribe: self.
	
	self 
		cleanUpTrait: TOne;
		cleanUpTrait: TTwo.
		
	Smalltalk organization removeCategory: self aCategory.
	(RPackageOrganizer default 
		packageNamed: self aCategory
		ifAbsent: [ ^ self ]) 
		unregister.