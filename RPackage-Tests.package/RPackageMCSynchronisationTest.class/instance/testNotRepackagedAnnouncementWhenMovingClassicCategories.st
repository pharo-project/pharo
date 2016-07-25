to move to a simple RPackage test case
testNotRepackagedAnnouncementWhenMovingClassicCategories

	| ann class firstPackage secondPackage|
	
	ann := nil.
	SystemAnnouncer uniqueInstance
		when: MethodRepackaged do: [ :a | ann := a ].
	
	self addXYCategory. 
	firstPackage := self organizer  packageNamed: #XXXXX.
	secondPackage := self organizer  packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.

	self createMethodNamed: 'stubMethod' inClass:  class inCategory: 'classic'.
	
	self assert: ann isNil.

	class organization classify: #stubMethod under: 'another classic one'.

	self assert: ann isNil.