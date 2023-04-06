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

{ #category : #running }
RBCopyPackageParametrizedTest >> addMethodWithRefTo: classRefName inClass: className [

	(RBAddMethodTransformation
		sourceCode: (String streamContents: [ : stream |
									stream << 'foo';
										cr;
										<< '	^ ';
										<< classRefName ])
		in: className asSymbol
		withProtocols: { #accessing })
			asRefactoring execute
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
		refactoring := RBInsertNewClassTransformation
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
		withClasses: #('RBFTTest1' 'RBFTTest2').
	self addMethodWithRefTo: 'RBFTTest2' inClass: 'RBFTTest1'
]

{ #category : #running }
RBCopyPackageParametrizedTest >> tearDown [

	(RBRefactoryChangeFactory instance removePackageNamed: 'RefactoringSmallPackageForTest')
		execute.
	super tearDown
]

{ #category : #'failure tests' }
RBCopyPackageParametrizedTest >> testBadName [
	self shouldFail: (self createRefactoringWithArguments:
		{#'Refactoring-Tests-Core' . #'Refactoring-Tests-Core'})
]

{ #category : #tests }
RBCopyPackageParametrizedTest >> testCopyPackageAndChangesCopyReferences [
	| refactoring aModel |

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

	self
		assert: ((aModel classNamed: #BarFTTest1) parseTreeForSelector: #foo)
		equals: (self parseMethod: 'foo
			^ BarFTTest2')
]

{ #category : #tests }
RBCopyPackageParametrizedTest >> testCopyPackageWithParameters [
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

{ #category : #'failure tests' }
RBCopyPackageParametrizedTest >> testExistingPackage [
	self shouldFail: (self createRefactoringWithArguments:
		{ #'Refactoring-Tests-Core' . #'Refactoring-Tests-Changes'})
]
