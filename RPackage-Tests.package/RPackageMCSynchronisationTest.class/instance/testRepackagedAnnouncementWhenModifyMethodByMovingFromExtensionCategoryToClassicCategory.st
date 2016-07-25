to move to a simple RPackage test case
testRepackagedAnnouncementWhenModifyMethodByMovingFromExtensionCategoryToClassicCategory

	| ann class firstPackage secondPackage|
	
	ann := nil.
	SystemAnnouncer uniqueInstance
		when: MethodRepackaged do: [ :a | ann := a ].
	
	self addXYCategory. 
	firstPackage := self organizer  packageNamed: #XXXXX.
	secondPackage := self organizer  packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass:  class inCategory: '*yyyyy'.
	
	class organization classify: #stubMethod under: 'classic one'.		

	self assert: ann notNil.
	self assert: ann methodRepackaged selector equals: #stubMethod.
	self assert: ann oldPackage equals: secondPackage.
	self assert: ann newPackage equals: firstPackage.