Class {
	#name : #RBCopyPackageParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-SingleParametrized'
}

{ #category : #'building suites' }
RBCopyPackageParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBCopyPackageRefactoring };
		yourself
]

{ #category : #accessing }
RBCopyPackageParametrizedTest >> constructor [ 
	^ #copyPackage:in:
]

{ #category : #accessing }
RBCopyPackageParametrizedTest >> createPackageNamed: packageName withClasses: aCollection [

	[ RPackageOrganizer default createPackageNamed: packageName ]
	on: RPackageConflictError
	do: [ ].

	aCollection do: [ : className |
		| refactoring |
		refactoring := RBInsertClassTransformation
						addClass: className
						superclass: #Object
						subclasses: #()
						category: packageName.
		refactoring asRefactoring execute ]
]

{ #category : #accessing }
RBCopyPackageParametrizedTest >> removePackageNamed: packageName [

	RPackageOrganizer default removePackageNamed: packageName
]

{ #category : #running }
RBCopyPackageParametrizedTest >> setUp [
	super setUp.
	model := self defaultNamespace new.
	self
		createPackageNamed: 'RefactoringSmallPackageForTest'
		withClasses: #('RBFTTest1' 'RBFTTest2')
]

{ #category : #running }
RBCopyPackageParametrizedTest >> tearDown [

	super tearDown.
	(RBRefactoryChangeFactory instance removePackageNamed: 'RefactoringSmallPackageForTest')
		execute.
]

{ #category : #'failure tests' }
RBCopyPackageParametrizedTest >> testBadName [
	self shouldFail: (self createRefactoringWithArguments:
		{#'Refactoring-Tests-Core' . #'Refactoring-Tests-Core'})
]

{ #category : #tests }
RBCopyPackageParametrizedTest >> testCopyPackage [
	| refactoring aModel |

	refactoring := (self createRefactoringWithArguments: { #'RefactoringSmallPackageForTest' . #'RefactoringCOPYSmallPackageForTest'}).
	self executeRefactoring: (refactoring classMappings: { #RBFTTest1 -> #BarFTTest1 . #RBFTTest2 -> #BarFTTest2 } asDictionary).
	aModel := refactoring model.
	self
		assert: (aModel includesClassNamed: #BarFTTest1)
		description: 'It test that class was copied'.
	self
		assert: (aModel includesClassNamed: #BarFTTest2)
		description: 'It test that class was copied'.
	self
		assert: (aModel includesClassNamed: #RBFTTest1)
		description: 'It test that original class where we copied from is still there'.
	self
		assert: (aModel includesClassNamed: #RBFTTest2)
		description: 'It test that original class where we copied from is still there'
]

{ #category : #tests }
RBCopyPackageParametrizedTest >> testCopyPackageAndChangesCopyReferences [
	| refactoring aModel |
	"We should use a small package for this tests and it should have references to its classes sso that we can check it.

	See https://github.com/pharo-project/pharo/issues/11524"
	self timeLimit: 2 minutes.
	refactoring := (self createRefactoringWithArguments: { #'RefactoringSmallPackageForTest' . #'RefactoringCOPYSmallPackageForTest'}).
	self executeRefactoring: (refactoring classMappings: { #RBFTTest1 -> #BarFTTest1 . #RBFTTest2 -> #BarFTTest2 } asDictionary).
	aModel := refactoring model.

	self
		assert: (aModel classNamed: #BarFTTest1) category
		equals: #'RefactoringCOPYSmallPackageForTest'.
	self
		assert: (aModel classNamed: #BarFTTest2) superclass name
		equals: #Object.

	"We should create a little package and this package should have a class pointing to another class of this package.
	and we should check that the refactored version points to the class copied version.
	This is what this version is doing:

	self  assert: ((aModel classNamed: #RBCoreClassesHelpCopy) classSide parseTreeFor: #pages)
		equals: (self parseMethod: 'pages
	^ #(RBASTClassesHelpCopy RBRefactoringClassesHelpCopy RBBrowserEnvironmentsClassesHelpCopy)')"
]

{ #category : #'failure tests' }
RBCopyPackageParametrizedTest >> testExistingPackage [
	self shouldFail: (self createRefactoringWithArguments:
		{ #'Refactoring-Tests-Core' . #'Refactoring-Tests-Changes'})
]
