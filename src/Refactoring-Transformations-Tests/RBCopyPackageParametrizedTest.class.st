Class {
	#name : 'RBCopyPackageParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'building suites' }
RBCopyPackageParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBCopyPackageRefactoring };
		yourself
]

{ #category : 'accessing' }
RBCopyPackageParametrizedTest >> constructor [
	^ #copyPackage:in:
]

{ #category : 'running' }
RBCopyPackageParametrizedTest >> setUp [

	super setUp.
	model := self defaultNamespaceClass new.

	#( #RBFTTest1 #RBFTTest2 ) do: [ :className |
		(RBInsertNewClassTransformation className: className)
			superclass: #Object;
			packageName: #RefactoringSmallPackageForTest;
			execute ].

	(RBAddMethodTransformation
		 sourceCode: 'foo
^ RBFTTest2'
		 in: #RBFTTest1
		 withProtocol: #accessing) execute
]

{ #category : 'running' }
RBCopyPackageParametrizedTest >> tearDown [

	(RBRefactoryChangeFactory instance removePackageNamed: #RefactoringSmallPackageForTest) execute.
	(RBRefactoryChangeFactory instance removePackageNamed: #RefactoringCOPYSmallPackageForTest) execute.
	super tearDown
]

{ #category : 'tests' }
RBCopyPackageParametrizedTest >> testCopyPackageAndChangesCopyReferences [

	| refactoring aModel |
	refactoring := self createRefactoringWithArguments: { #RefactoringSmallPackageForTest. #RefactoringCOPYSmallPackageForTest }.
	self executeRefactoring: (refactoring classMappings: {
				 (#RBFTTest1 -> #BarFTTest1).
				 (#RBFTTest2 -> #BarFTTest2) } asDictionary).
	aModel := refactoring model.

	self assert: (aModel classNamed: #BarFTTest1) packageName equals: #RefactoringCOPYSmallPackageForTest.
	self assert: (aModel classNamed: #BarFTTest2) superclass name equals: #Object.

	self assert: ((aModel classNamed: #BarFTTest1) parseTreeForSelector: #foo) equals: (self parseMethod: 'foo
			^ BarFTTest2')
]

{ #category : 'tests' }
RBCopyPackageParametrizedTest >> testCopyPackageWithParameters [

	| refactoring aModel |
	refactoring := self createRefactoringWithArguments: { #RefactoringSmallPackageForTest. #RefactoringCOPYSmallPackageForTest }.
	self executeRefactoring: (refactoring classMappings: {
				 (#RBFTTest1 -> #BarFTTest1).
				 (#RBFTTest2 -> #BarFTTest2) } asDictionary).
	aModel := refactoring model.
	self assert: (aModel includesClassNamed: #BarFTTest1) description: 'It test that class was copied'.
	self assert: (aModel includesClassNamed: #BarFTTest2) description: 'It test that class was copied'.
	self assert: (aModel includesClassNamed: #RBFTTest1) description: 'It test that original class where we copied from is still there'.
	self assert: (aModel includesClassNamed: #RBFTTest2) description: 'It test that original class where we copied from is still there'
]

{ #category : 'failure tests' }
RBCopyPackageParametrizedTest >> testFailureBadName [

	self shouldFail: (self createRefactoringWithArguments: { #'Refactoring-Core-Tests'. #'Refactoring-Core-Tests' })
]

{ #category : 'failure tests' }
RBCopyPackageParametrizedTest >> testFailureExistingPackage [

	self shouldFail: (self createRefactoringWithArguments: { #'Refactoring-Core-Tests'. #'Refactoring-Changes-Tests' })
]
