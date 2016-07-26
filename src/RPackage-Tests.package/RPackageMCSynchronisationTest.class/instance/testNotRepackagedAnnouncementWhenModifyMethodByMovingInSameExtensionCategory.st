to move to a simple RPackage test case
testNotRepackagedAnnouncementWhenModifyMethodByMovingInSameExtensionCategory
	
	| ann class firstPackage secondPackage|
	
	ann := nil.
	SystemAnnouncer uniqueInstance
		when: MethodRepackaged do: [ :a | ann := a ].
	
	self addXYCategory. 
	firstPackage := self organizer  packageNamed: #XXXXX.
	secondPackage := self organizer  packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.

	self createMethodNamed: 'stubMethod' inClass:  class inCategory: '*yyyyy'.
	
	self assert: ann isNil.

	class organization classify: #stubMethod under: '*yyyyy-suncategory'.

	self assert: ann isNil.