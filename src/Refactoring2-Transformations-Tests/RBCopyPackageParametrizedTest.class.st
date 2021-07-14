Class {
	#name : #RBCopyPackageParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBCopyPackageParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBCopyPackageRefactoring };
		yourself
]

{ #category : #'failure tests' }
RBCopyPackageParametrizedTest >> constructor [ 
	^ #copyPackage:in:
]

{ #category : #'failure tests' }
RBCopyPackageParametrizedTest >> testBadName [
	self shouldFail: (self createRefactoringWithArguments: 
		{#'Refactoring-Tests-Core' . #'Refactoring-Tests-Core'}).
]

{ #category : #tests }
RBCopyPackageParametrizedTest >> testCopyPackage [
	| refactoring aModel |
	refactoring := (self createRefactoringWithArguments: 
		{ #'Refactoring-Tests-Changes' . #'Refactoring-Tests-Changes1'}).
	aModel := refactoring model.
	
	self executeRefactoring: refactoring.
	
	self assert: (aModel packageNamed: #'Refactoring-Tests-Changes1') isNotNil.
	self assert: (aModel classNamed: #RBRefactoringChangeTestCopy) category equals: #'Refactoring-Tests-Changes1'.
	self assert: (aModel classNamed: #RBRefactoringChangeTestCopy) instanceVariableNames asArray
		equals: #( 'changes' 'workingEnvironment' 'changeFactory').
	self assert: (aModel classNamed: #RBRefactoringChangeManagerTestCopy) category equals: #'Refactoring-Tests-Changes1'.
	
]

{ #category : #tests }
RBCopyPackageParametrizedTest >> testCopyPackageAndChangesCopyReferences [
	| refactoring aModel |
	self timeLimit: 2 minutes.
	refactoring := (self createRefactoringWithArguments: 
		{ #'Refactoring-Help' . #'Refactoring-Help1'}).
	aModel := refactoring model.
	
	self executeRefactoring: refactoring.
	
	self assert: (aModel classNamed: #RBRefactoringClassesHelpCopy) category 
		equals: #'Refactoring-Help1'.
	self assert: (aModel classNamed: #RBRefactoringClassesHelpCopy) superclass name 
		equals: #RBClassesHelpCopy.
	self assert: (aModel classNamed: #RBRefactoringClassesHelpCopy) superclass name 
		equals: #RBClassesHelpCopy.
	self assert: ((aModel classNamed: #RBCoreClassesHelpCopy) classSide parseTreeFor: #pages) 
		equals: (self parseMethod: 'pages
	^ #(RBASTClassesHelpCopy RBRefactoringClassesHelpCopy RBBrowserEnvironmentsClassesHelpCopy)')
]

{ #category : #'failure tests' }
RBCopyPackageParametrizedTest >> testExistingPackage [
	self shouldFail: (self createRefactoringWithArguments: 
		{ #'Refactoring-Tests-Core' . #'Refactoring-Tests-Changes'})
]
