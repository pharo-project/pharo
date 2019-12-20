"
SUnit tests for RPackage category synchronization
"
Class {
	#name : #RPackageCategorySynchronisationTest,
	#superclass : #RPackageMCSynchronisationTest,
	#category : #'Monticello-Tests-RPackage'
}

{ #category : #'tests - operations on categories' }
RPackageCategorySynchronisationTest >> testAddCategoryCreatesPackage [
	"test that when we add a category that does not yet correspond to an existing package, a new package is created"
		
	self flag: #pharoTodo. "To be removed when Categories are kicked out"
	
	self addXCategory.
	self assert: (self organizer includesPackageNamed: #XXXXX)
]

{ #category : #'tests - operations on categories' }
RPackageCategorySynchronisationTest >> testAddCategoryIsAlreadyAPackageDoesNotCreatePackage [
	"test that when we add a category corresponding to an existing package, we do not create a second package"

	| tmpPackage |
	self flag: #pharoTodo.	"To be removed when Categories are kicked out"

	self addXCategory.
	tmpPackage := self organizer packageNamed: #XXXXX.
	self addXCategory.
	self assert: tmpPackage equals: (self organizer packageNamed: #XXXXX)
]

{ #category : #'tests - operations on categories' }
RPackageCategorySynchronisationTest >> testRenameCategoryAlsoRenameAllExtensionProtocols [
	"test that when we rename a category, all corresponding extension protocols in the system are renamed"

	| XPackage YPackage ZPackage classInY classInZ |
	self addXCategory.
	self addYCategory.
	self addZCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	ZPackage := self organizer packageNamed: #ZZZZZ.

	classInY := self createNewClassNamed: 'ClassInYPackage' inCategory: 'YYYYY'.
	classInZ := self createNewClassNamed: 'ClassInZPackage' inCategory: 'ZZZZZ'.

	self createMethodNamed: 'extensionFromXInClassInY' inClass: classInY inCategory: '*XXXXX'.
	self createMethodNamed: 'longNameExtensionFromXInClassInY' inClass: classInY inCategory: '*XXXXX-subcategory'.
	self createMethodNamed: 'extensionFromXInClassInZ' inClass: classInZ inCategory: '*XXXXX'.

	Smalltalk organization renameCategory: 'XXXXX' toBe: 'NewCategoryName'.

	self assert: XPackage name equals: 'NewCategoryName'.
	self assert: (classInY >> #extensionFromXInClassInY) category equals: '*NewCategoryName'.
	self assert: (classInY >> #longNameExtensionFromXInClassInY) category equals: '*NewCategoryName'.
	self assert: (classInZ >> #extensionFromXInClassInZ) category equals: '*NewCategoryName'
]

{ #category : #'tests - operations on categories' }
RPackageCategorySynchronisationTest >> testRenameCategoryChangeTheNameOfThePackage [
	"test that when we rename a category, the RPackage corresponding is updated with this new name"

	| XPackage |
	self addXCategory.
	XPackage := self organizer packageNamed: #XXXXX.

	Smalltalk organization renameCategory: 'XXXXX' toBe: 'YYYYY'.
	self assert: XPackage name equals: 'YYYYY'
]

{ #category : #'tests - operations on categories' }
RPackageCategorySynchronisationTest >> testRenameCategoryUpdateTheOrganizer [
	"test that when we rename a category, the organizer dictionary is update with this new name, so that we can access the package with this new name as key"

	| XPackage |
	self addXCategory.

	XPackage := self organizer packageNamed: #XXXXX.
	Smalltalk organization renameCategory: 'XXXXX' toBe: 'YYYYY'.
	self assert: (self organizer packageNamed: 'YYYYY' asSymbol) equals: XPackage.
	self deny: (self organizer includesPackageNamed: #XXXXX)
]

{ #category : #'tests - operations on categories' }
RPackageCategorySynchronisationTest >> testRenameUnknownCategoryCreatesNewRPackage [
	"test that when we rename a category that is not registered in RPackage , it does not raise errors and simply create a new package. We need this behaviour as for now, create a new category with the class browser does not emit the corrects events, and therefore RPackage can not be directly updated"
	
	SystemAnnouncer uniqueInstance suspendAllWhile: [
		self addXCategory.
		].
	self deny: (self organizer includesPackageNamed: #XXXXX).
	Smalltalk organization renameCategory: 'XXXXX' toBe: 'YYYYY'.
	self assert: (self organizer includesPackageNamed: #YYYYY).
]
