tests - recategorizing class
testRecategorizeClassRaisesClassRepackagedEvent
	"test that when we recategorize a class, the organizer is updated so that the class name point the the new RPackage"
	
	| XPackage YPackage class ann |
	
	ann := nil.
	SystemAnnouncer uniqueInstance
		when: ClassRepackaged do: [ :a |
			ann := a.
		].
	
	self addXCategory.
	self addYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	class category: 'YYYYY'.	
	
	self assert: ann notNil.
	self assert: ann classRepackaged equals: class.
	self assert: ann oldPackage equals: XPackage.
	self assert: ann newPackage equals: YPackage.