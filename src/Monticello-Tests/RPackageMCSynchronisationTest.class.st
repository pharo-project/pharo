"
I had some problems using the 'ensure:' method. There was some conflicts with the organizer used for the test when trying to clean the classes, categories and packages generated for the test. 
There is something I don't get with this method. Therefore I choosed to clean by default all possible generated classes, categories and packages after each test using the tearDown method.

For now, all that was in the 'ensure:' methods has been commented.
Look for example at:
    testAddMethodInClassicCategoryAddMethodToTheParentPackageOfItsClass
and try to uncomment it
"
Class {
	#name : #RPackageMCSynchronisationTest,
	#superclass : #RPackageTestCase,
	#instVars : [
		'emptyOrganizer',
		'oldAnnouncer'
	],
	#category : #'Monticello-Tests-RPackage'
}

{ #category : #utilities }
RPackageMCSynchronisationTest >> addXMatchCategory [
	self organizer addCategory: 'XXXXX-YYYY'.
]

{ #category : #utilities }
RPackageMCSynchronisationTest >> addYCategory [

	self organizer addCategory: 'YYYYY'.
			
]

{ #category : #utilities }
RPackageMCSynchronisationTest >> addZCategory [

	self organizer addCategory: 'ZZZZZ'.
			
]

{ #category : #setup }
RPackageMCSynchronisationTest >> cleanClassesPackagesAndCategories [

	self organizer environment
		removeClassNamed: 'NewClass';
		removeClassNamed: 'RPackageNewStubClass';
		removeClassNamed: 'RPackageOldStubClass';
		removeClassNamed: 'Foo';
		removeClassNamed: 'FooOther';
		removeClassNamed: 'NewTrait';
		removeClassNamed: 'ClassInYPackage';
		removeClassNamed: 'ClassInZPackage'.
	self organizer
		removeCategory: 'Zork';
		removeCategory: 'XXXXX';
		removeCategory: 'XXXXX-YYYY';
		removeCategory: 'XXXX';
		removeCategory: 'YYYYY';
		removeCategory: 'ZZZZZ';
		removeCategory: 'FooPackage-Core';
		removeCategory: 'FooPackage-Other';
		removeCategory: 'FooPackage';
		removeCategory: 'OriginalCategory';
		removeCategory: 'NewCategoryName';
		removeCategory: 'Y'.
	#( 'OriginalCategory' 'XXXXX' 'XXXX' 'YYYYY' 'YYYY' 'Yyyyy' 'YyYyY' 'Y' 'ZZZZZ' 'Zzzzz' 'FooPackage-Core' 'FooPackage-Other' 'FooPackage' 'Zork' ) do: [
		:packageName |
		self allWorkingCopies
			detect: [ :each | each packageName = packageName ]
			ifFound: [ :mCPackage | mCPackage unregister ] ]
]

{ #category : #private }
RPackageMCSynchronisationTest >> createMethodNamed: methodName inClass: aClass inCategory: aCategoryName [ 

	^ aClass compile: (methodName, ' ^nil') classified: aCategoryName.
]

{ #category : #private }
RPackageMCSynchronisationTest >> createMethodNamed: methodName inClassSideOfClass: aClass inCategory: aCategoryName [ 

	^ aClass classSide compile: (methodName, ' ^nil') classified: aCategoryName.
]

{ #category : #accessing }
RPackageMCSynchronisationTest >> emptyOrganizer [ 

	^ emptyOrganizer
]

{ #category : #utilities }
RPackageMCSynchronisationTest >> ensureXPackage [

	^ self organizer ensurePackage: 'XXXXX'
]

{ #category : #'announcer handling' }
RPackageMCSynchronisationTest >> initializeAnnouncers [

	oldAnnouncer := MCWorkingCopy announcer.
	MCWorkingCopy announcer: SystemAnnouncer uniqueInstance
]

{ #category : #'announcer handling' }
RPackageMCSynchronisationTest >> restoreAnnouncers [

	MCWorkingCopy announcer: oldAnnouncer
]

{ #category : #running }
RPackageMCSynchronisationTest >> runCase [

	[
	self initializeAnnouncers.
	super runCase ] ensure: [ self restoreAnnouncers ]
]

{ #category : #running }
RPackageMCSynchronisationTest >> setUp [

	super setUp.

	emptyOrganizer := self organizer
		                  debuggingName: 'MCSynchronisation Package Organizer';
		                  yourself.
	emptyOrganizer ensurePackage: 'as yet unclassified'.

	Author fullName ifNil: [ Author fullName: 'Tester' ]
]

{ #category : #running }
RPackageMCSynchronisationTest >> tearDown [

	MCWorkingCopy removeDependent: self emptyOrganizer.
	self cleanClassesPackagesAndCategories.
	SystemAnnouncer uniqueInstance unsubscribe: self.
	super tearDown
]

{ #category : #tests }
RPackageMCSynchronisationTest >> testCategoryMatching [

	self assert: ('Compression-Archives' isCategoryOf: 'Compression').
	self deny: ('Compression' isCategoryOf: 'Compression-Archives').
	self assert: ('Compression' isCategoryOf: 'Compression').
	self assert: ('Compression-Arh' isCategoryOf: 'Compression').
	self deny: ('XXXX' isCategoryOf: 'Compression-Archives')
]

{ #category : #tests }
RPackageMCSynchronisationTest >> testIsDefinedAsPackageOrSubPackageInMC [
	"test whether a given category is defined as a MC package or whether it is defined as top part of package"
	
	self assert: (self organizer isDefinedAsPackageOrSubPackageInMC: #'Compression').
	self deny: (self organizer isDefinedAsPackageOrSubPackageInMC: #'Compression-Archives').
	"Indeed compression-archives is a category but not a MC package"
	
	self assert: (self organizer isDefinedAsPackageOrSubPackageInMC: #'Collections').
	"#Collection is not defined as a package but as a package #'Collections-Unordered'"
	self assert: (self organizer isDefinedAsPackageOrSubPackageInMC: #'Collections-Unordered').
	self deny: (self organizer isDefinedAsPackageOrSubPackageInMC: #'Collections-Unordered-Test').
]

{ #category : #'to move to a simple RPackage test case' }
RPackageMCSynchronisationTest >> testNotRepackagedAnnouncementWhenModifyMethodByMovingInSameExtensionCategory [

	| ann class firstPackage secondPackage |
	ann := nil.
	SystemAnnouncer uniqueInstance when: MethodRepackaged do: [ :a | ann := a ] for: self.

	firstPackage := self ensureXPackage.
	self addYCategory.
	secondPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.

	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*yyyyy'.

	self assert: ann isNil.

	class classify: #stubMethod under: '*yyyyy-suncategory'.

	self assert: ann isNil
]

{ #category : #'to move to a simple RPackage test case' }
RPackageMCSynchronisationTest >> testNotRepackagedAnnouncementWhenMovingClassicCategories [

	| ann class firstPackage secondPackage |
	ann := nil.
	SystemAnnouncer uniqueInstance when: MethodRepackaged do: [ :a | ann := a ] for: self.

	firstPackage := self ensureXPackage.
	self addYCategory.
	secondPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.

	self createMethodNamed: 'stubMethod' inClass: class inCategory: 'classic'.

	self assert: ann isNil.

	class classify: #stubMethod under: 'another classic one'.

	self assert: ann isNil
]

{ #category : #'to move to a simple RPackage test case' }
RPackageMCSynchronisationTest >> testRepackagedAnnouncementWhenModifyMethodByMovingFromClassicCategoryToExtensionCategory [

	| ann class firstPackage secondPackage |
	ann := nil.
	SystemAnnouncer uniqueInstance when: MethodRepackaged do: [ :a | ann := a ] for: self.

	firstPackage := self ensureXPackage.
	self addYCategory.
	secondPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: 'classic category'.

	class classify: #stubMethod under: '*yyyyy'.

	self assert: ann notNil.
	self assert: ann methodRepackaged selector equals: #stubMethod.
	self assert: ann oldPackage equals: firstPackage.
	self assert: ann newPackage equals: secondPackage.
	ann := nil.

	class classify: #stubMethod under: '*yyyyy-suncategory'.

	self assert: ann isNil
]

{ #category : #'to move to a simple RPackage test case' }
RPackageMCSynchronisationTest >> testRepackagedAnnouncementWhenModifyMethodByMovingFromDifferentExtensionCategories [

	| ann class firstPackage secondPackage thirdPackage |
	ann := nil.
	SystemAnnouncer uniqueInstance when: MethodRepackaged do: [ :a | ann := a ] for: self.

	firstPackage := self ensureXPackage.
	self
		addYCategory;
		addZCategory.
	secondPackage := self organizer packageNamed: #YYYYY.
	thirdPackage := self organizer packageNamed: #ZZZZZ.

	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*yyyyy'.

	class classify: #stubMethod under: '*zzzzz'.

	self assert: ann notNil.
	self assert: ann methodRepackaged selector equals: #stubMethod.
	self assert: ann oldPackage equals: secondPackage.
	self assert: ann newPackage equals: thirdPackage
]

{ #category : #'to move to a simple RPackage test case' }
RPackageMCSynchronisationTest >> testRepackagedAnnouncementWhenModifyMethodByMovingFromExtensionCategoryToClassicCategory [

	| ann class firstPackage secondPackage |
	ann := nil.
	SystemAnnouncer uniqueInstance when: MethodRepackaged do: [ :a | ann := a ] for: self.

	firstPackage := self ensureXPackage.
	self addYCategory.
	secondPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*yyyyy'.

	class classify: #stubMethod under: 'classic one'.

	self assert: ann notNil.
	self assert: ann methodRepackaged selector equals: #stubMethod.
	self assert: ann oldPackage equals: secondPackage.
	self assert: ann newPackage equals: firstPackage
]
