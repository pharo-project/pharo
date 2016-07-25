to move to a simple RPackage test case
testRepackagedAnnouncementWhenModifyMethodByMovingFromClassicCategoryToExtensionCategory
	
	| ann class firstPackage secondPackage|
	
	ann := nil.
	SystemAnnouncer uniqueInstance
		when: MethodRepackaged do: [ :a | ann := a ].
	
	self addXYCategory. 
	firstPackage := self organizer  packageNamed: #XXXXX.
	secondPackage := self organizer  packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass:  class inCategory: 'classic category'.
	
	class organization classify: #stubMethod under: '*yyyyy'.		

	self assert: ann notNil.
	self assert: ann methodRepackaged selector equals: #stubMethod.
	self assert: ann oldPackage equals: firstPackage.
	self assert: ann newPackage equals: secondPackage.
	ann := nil.

	class organization classify: #stubMethod under: '*yyyyy-suncategory'.

	self assert: ann isNil.