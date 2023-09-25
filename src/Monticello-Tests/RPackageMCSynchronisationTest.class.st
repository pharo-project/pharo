"
I had some problems using the 'ensure:' method. There was some conflicts with the organizer used for the test when trying to clean the classes, categories and packages generated for the test. 
There is something I don't get with this method. Therefore I choosed to clean by default all possible generated classes, categories and packages after each test using the tearDown method.

For now, all that was in the 'ensure:' methods has been commented.
Look for example at:
    testAddMethodInClassicCategoryAddMethodToTheParentPackageOfItsClass
and try to uncomment it
"
Class {
	#name : 'RPackageMCSynchronisationTest',
	#superclass : 'RPackageTestCase',
	#instVars : [
		'emptyOrganizer',
		'oldAnnouncer'
	],
	#category : 'Monticello-Tests-RPackage',
	#package : 'Monticello-Tests',
	#tag : 'RPackage'
}

{ #category : 'accessing' }
RPackageMCSynchronisationTest >> allWorkingCopies [

	^ MCWorkingCopy allWorkingCopies
]

{ #category : 'setup' }
RPackageMCSynchronisationTest >> cleanClassesPackagesAndCategories [

	#( 'OriginalCategory' 'XXXXX' 'XXXX' 'YYYYY' 'YYYY' 'Yyyyy' 'YyYyY' 'Y' 'ZZZZZ' 'Zzzzz' 'FooPackage-Core' 'FooPackage-Other' 'FooPackage' 'Zork' ) do: [
		:packageName |
		self allWorkingCopies
			detect: [ :each | each packageName = packageName ]
			ifFound: [ :mCPackage | mCPackage unregister ] ]
]

{ #category : 'private' }
RPackageMCSynchronisationTest >> createMethodNamed: methodName inClass: aClass inCategory: aCategoryName [ 

	^ aClass compile: (methodName, ' ^nil') classified: aCategoryName.
]

{ #category : 'private' }
RPackageMCSynchronisationTest >> createMethodNamed: methodName inClassSideOfClass: aClass inCategory: aCategoryName [ 

	^ aClass classSide compile: (methodName, ' ^nil') classified: aCategoryName.
]

{ #category : 'accessing' }
RPackageMCSynchronisationTest >> emptyOrganizer [ 

	^ emptyOrganizer
]

{ #category : 'utilities' }
RPackageMCSynchronisationTest >> ensureTagYinX [

	self organizer ensureTagNamed: #YYYY inPackageNamed: #XXXXX
]

{ #category : 'utilities' }
RPackageMCSynchronisationTest >> ensureXPackage [

	^ self organizer ensurePackage: #XXXXX
]

{ #category : 'utilities' }
RPackageMCSynchronisationTest >> ensureYPackage [

	^ self organizer ensurePackage: #YYYYY
]

{ #category : 'utilities' }
RPackageMCSynchronisationTest >> ensureZPackage [

	^ self organizer ensurePackage: #ZZZZZ
]

{ #category : 'announcer handling' }
RPackageMCSynchronisationTest >> initializeAnnouncers [

	oldAnnouncer := MCWorkingCopy announcer.
	MCWorkingCopy announcer: SystemAnnouncer uniqueInstance
]

{ #category : 'announcer handling' }
RPackageMCSynchronisationTest >> restoreAnnouncers [

	MCWorkingCopy announcer: oldAnnouncer
]

{ #category : 'running' }
RPackageMCSynchronisationTest >> runCase [

	[
	self initializeAnnouncers.
	super runCase ] ensure: [ self restoreAnnouncers ]
]

{ #category : 'running' }
RPackageMCSynchronisationTest >> setUp [

	super setUp.

	emptyOrganizer := self organizer.
	emptyOrganizer ensurePackage: 'as yet unclassified'.

	Author fullName ifNil: [ Author fullName: 'Tester' ]
]

{ #category : 'running' }
RPackageMCSynchronisationTest >> tearDown [

	MCWorkingCopy removeDependent: self emptyOrganizer.
	self cleanClassesPackagesAndCategories.
	SystemAnnouncer uniqueInstance unsubscribe: self.
	super tearDown
]

{ #category : 'to move to a simple RPackage test case' }
RPackageMCSynchronisationTest >> testNotRepackagedAnnouncementWhenModifyMethodByMovingInSameExtensionCategory [

	| ann class |
	SystemAnnouncer uniqueInstance when: MethodRepackaged do: [ :a | ann := a ] for: self.

	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.

	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*yyyyy'.

	self assert: ann isNil.

	class classify: #stubMethod under: '*yyyyy-suncategory'.

	self assert: ann isNil
]

{ #category : 'to move to a simple RPackage test case' }
RPackageMCSynchronisationTest >> testNotRepackagedAnnouncementWhenMovingClassicCategories [

	| ann class |
	SystemAnnouncer uniqueInstance when: MethodRepackaged do: [ :a | ann := a ] for: self.

	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.

	self createMethodNamed: 'stubMethod' inClass: class inCategory: 'classic'.

	self assert: ann isNil.

	class classify: #stubMethod under: 'another classic one'.

	self assert: ann isNil
]

{ #category : 'to move to a simple RPackage test case' }
RPackageMCSynchronisationTest >> testRepackagedAnnouncementWhenModifyMethodByMovingFromClassicCategoryToExtensionCategory [

	| ann class firstPackage secondPackage |
	SystemAnnouncer uniqueInstance when: MethodRepackaged do: [ :a | ann := a ] for: self.

	firstPackage := self ensureXPackage.
	secondPackage := self ensureYPackage.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: 'classic category'.

	class classify: #stubMethod under: '*yyyyy'.

	self assert: ann isNotNil.
	self assert: ann methodRepackaged selector equals: #stubMethod.
	self assert: ann oldPackage equals: firstPackage.
	self assert: ann newPackage equals: secondPackage.
	ann := nil.

	class classify: #stubMethod under: '*yyyyy-suncategory'.

	self assert: ann isNil
]

{ #category : 'to move to a simple RPackage test case' }
RPackageMCSynchronisationTest >> testRepackagedAnnouncementWhenModifyMethodByMovingFromDifferentExtensionCategories [

	| ann class secondPackage thirdPackage |
	SystemAnnouncer uniqueInstance when: MethodRepackaged do: [ :a | ann := a ] for: self.

	self ensureXPackage.
	secondPackage := self ensureYPackage.
	thirdPackage := self ensureZPackage.

	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*yyyyy'.

	class classify: #stubMethod under: '*zzzzz'.

	self assert: ann isNotNil.
	self assert: ann methodRepackaged selector equals: #stubMethod.
	self assert: ann oldPackage equals: secondPackage.
	self assert: ann newPackage equals: thirdPackage
]

{ #category : 'to move to a simple RPackage test case' }
RPackageMCSynchronisationTest >> testRepackagedAnnouncementWhenModifyMethodByMovingFromExtensionCategoryToClassicCategory [

	| ann class firstPackage secondPackage |
	SystemAnnouncer uniqueInstance when: MethodRepackaged do: [ :a | ann := a ] for: self.

	firstPackage := self ensureXPackage.
	secondPackage := self ensureYPackage.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*yyyyy'.

	class classify: #stubMethod under: 'classic one'.

	self assert: ann isNotNil.
	self assert: ann methodRepackaged selector equals: #stubMethod.
	self assert: ann oldPackage equals: secondPackage.
	self assert: ann newPackage equals: firstPackage
]
