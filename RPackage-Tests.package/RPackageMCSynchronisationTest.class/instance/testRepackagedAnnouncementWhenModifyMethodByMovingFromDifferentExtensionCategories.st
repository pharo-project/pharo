to move to a simple RPackage test case
testRepackagedAnnouncementWhenModifyMethodByMovingFromDifferentExtensionCategories

	| ann class firstPackage secondPackage thirdPackage |
	
	ann := nil.
	SystemAnnouncer uniqueInstance
		when: MethodRepackaged do: [ :a | ann := a ].
	
	self addXYZCategory. 
	firstPackage := self organizer  packageNamed: #XXXXX.
	secondPackage := self organizer packageNamed: #YYYYY.
	thirdPackage := self organizer packageNamed: #ZZZZZ.
	
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass:  class inCategory: '*yyyyy'.
	
	class organization classify: #stubMethod under: '*zzzzz'.

	self assert: ann notNil.
	self assert: ann methodRepackaged selector equals: #stubMethod.
	self assert: ann oldPackage equals: secondPackage.
	self assert: ann newPackage equals: thirdPackage.