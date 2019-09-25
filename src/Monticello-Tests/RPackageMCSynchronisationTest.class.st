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
RPackageMCSynchronisationTest >> addXCategory [
	testingEnvironment organization addCategory: 'XXXXX'.
]

{ #category : #utilities }
RPackageMCSynchronisationTest >> addXMatchCategory [
	testingEnvironment organization addCategory: 'XXXXX-YYYY'.
]

{ #category : #utilities }
RPackageMCSynchronisationTest >> addXYCategory [

	self addXCategory.
	self addYCategory.
			
]

{ #category : #utilities }
RPackageMCSynchronisationTest >> addXYZCategory [

	self addXCategory.
	self addYCategory.
	self addZCategory.
			
]

{ #category : #utilities }
RPackageMCSynchronisationTest >> addYCategory [

	testingEnvironment organization addCategory: 'YYYYY'.
			
]

{ #category : #utilities }
RPackageMCSynchronisationTest >> addZCategory [

	testingEnvironment organization addCategory: 'ZZZZZ'.
			
]

{ #category : #setup }
RPackageMCSynchronisationTest >> cleanClassesPackagesAndCategories [

		|mCPackage|
		Smalltalk removeClassNamed: 'NewClass'.
		Smalltalk removeClassNamed: 'RPackageNewStubClass'.
		Smalltalk removeClassNamed: 'RPackageOldStubClass'.
		Smalltalk removeClassNamed: 'Foo'.
		Smalltalk removeClassNamed: 'FooOther'.
		Smalltalk removeClassNamed: 'NewTrait'.
		Smalltalk removeClassNamed: 'ClassInYPackage'.
		Smalltalk removeClassNamed: 'ClassInZPackage'.
		Smalltalk organization removeCategory: 'Zork'.
		Smalltalk organization removeCategory: 'XXXXX'.
		Smalltalk organization removeCategory: 'XXXXX-YYYY'.
		Smalltalk organization removeCategory: 'XXXX'.
		Smalltalk organization removeCategory: 'YYYYY'.
		Smalltalk organization removeCategory: 'ZZZZZ'.
		Smalltalk organization removeCategory: 'FooPackage-Core'.
		Smalltalk organization removeCategory: 'FooPackage-Other'.
		Smalltalk organization removeCategory: 'FooPackage'.
		Smalltalk organization removeCategory: 'OriginalCategory'.
		Smalltalk organization removeCategory: 'NewCategoryName'.
		Smalltalk organization removeCategory: 'Y'.
		mCPackage := (self allManagers detect: [:each | each packageName = 'OriginalCategory'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].
		mCPackage := (self allManagers detect: [:each | each packageName = 'XXXXX'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].
		mCPackage := (self allManagers detect: [:each | each packageName = 'XXXX'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].
		mCPackage := (self allManagers detect: [:each | each packageName = 'YYYYY'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].
		mCPackage := (self allManagers detect: [:each | each packageName = 'YYYY'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].
		mCPackage := (self allManagers detect: [:each | each packageName = 'Yyyyy'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].	
		mCPackage := (self allManagers detect: [:each | each packageName = 'YyYyY'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].
		mCPackage := (self allManagers detect: [:each | each packageName = 'Y'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].
		mCPackage := (self allManagers detect: [:each | each packageName = 'ZZZZZ'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].	
		mCPackage := (self allManagers detect: [:each | each packageName = 'Zzzzz'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].
		mCPackage := (self allManagers detect: [:each | each packageName = 'FooPackage-Core'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].
		mCPackage := (self allManagers detect: [:each | each packageName = 'FooPackage-Other'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].
		mCPackage := (self allManagers detect: [:each | each packageName = 'FooPackage'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].
		mCPackage := (self allManagers detect: [:each | each packageName = 'Zork'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].
			
]

{ #category : #private }
RPackageMCSynchronisationTest >> createMethodNamed: methodName inClass: aClass inCategory: aCategoryName [ 

	^ aClass compile: (methodName, ' ^nil') classified: aCategoryName.
]

{ #category : #private }
RPackageMCSynchronisationTest >> createMethodNamed: methodName inClassSideOfClass: aClass inCategory: aCategoryName [ 

	^ aClass classSide compile: (methodName, ' ^nil') classified: aCategoryName.
]

{ #category : #private }
RPackageMCSynchronisationTest >> createNewClassNamed: aName [
	
	| cls |
	cls := Object subclass: aName
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'RPackageTest'.
	^ cls
]

{ #category : #private }
RPackageMCSynchronisationTest >> createNewClassNamed: aName inCategory: aCategoryName [
	
	| cls |
	cls := Object subclass: aName asSymbol
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: aCategoryName.
	^ cls
]

{ #category : #private }
RPackageMCSynchronisationTest >> createNewTraitNamed: aName inCategory: aCategoryName [
	
	| trait |
	trait := Trait named: aName asSymbol
			uses: {}
			category: aCategoryName.
	^ trait
]

{ #category : #accessing }
RPackageMCSynchronisationTest >> emptyOrganizer [ 

	^ emptyOrganizer
]

{ #category : #'announcer handling' }
RPackageMCSynchronisationTest >> initializeAnnouncers [
	super initializeAnnouncers.
	
	oldAnnouncer := MCWorkingCopy announcer.
	MCWorkingCopy announcer: announcerForTest.
]

{ #category : #'announcer handling' }
RPackageMCSynchronisationTest >> restoreAnnouncers [
	super restoreAnnouncers.
	MCWorkingCopy announcer: oldAnnouncer.
]

{ #category : #running }
RPackageMCSynchronisationTest >> setUp [ 
	super setUp.
	
	emptyOrganizer := self organizer debuggingName: 'MCSynchronisation Package Organizer'; yourself.
	emptyOrganizer registerPackage: (self packageClass named: 'as yet unclassified').

	Author fullName 
		ifNil: [ Author fullName: 'Tester' ].
]

{ #category : #running }
RPackageMCSynchronisationTest >> tearDown [
	MCWorkingCopy removeDependent: self emptyOrganizer.
	self cleanClassesPackagesAndCategories.
	super tearDown.
]

{ #category : #tests }
RPackageMCSynchronisationTest >> testCategoryMatching [
	
	self assert: (self organizer category: 'Compression-Archives' matches: 'Compression').
	self deny: (self organizer category: 'Compression' matches: 'Compression-Archives').
	self assert: (self organizer category: 'Compression' matches: 'Compression').
	self assert: (self organizer category: 'Compression-Arh' matches: 'Compression').
	self deny: (self organizer category: 'XXXX' matches: 'Compression-Archives').
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
]

{ #category : #'to move to a simple RPackage test case' }
RPackageMCSynchronisationTest >> testNotRepackagedAnnouncementWhenMovingClassicCategories [

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
]

{ #category : #'to move to a simple RPackage test case' }
RPackageMCSynchronisationTest >> testRepackagedAnnouncementWhenModifyMethodByMovingFromClassicCategoryToExtensionCategory [
	
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
]

{ #category : #'to move to a simple RPackage test case' }
RPackageMCSynchronisationTest >> testRepackagedAnnouncementWhenModifyMethodByMovingFromDifferentExtensionCategories [

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
]

{ #category : #'to move to a simple RPackage test case' }
RPackageMCSynchronisationTest >> testRepackagedAnnouncementWhenModifyMethodByMovingFromExtensionCategoryToClassicCategory [

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
]
