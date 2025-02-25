Class {
	#name : 'MyRunnerPresenterTreeTest',
	#superclass : 'TestCase',
	#instVars : [
		'testRunnerUI'
	],
	#category : 'TestRunner-UI-Tests',
	#package : 'TestRunner-UI-Tests'
}

{ #category : 'running' }
MyRunnerPresenterTreeTest >> setUp [

	super setUp.
	testRunnerUI := TestRunnerTreePresenter new
]

{ #category : 'running' }
MyRunnerPresenterTreeTest >> testANewTestRunnerPresenterShouldntHaveAnyClassInClassesPresenter [

	self assert:
		testRunnerUI classesPresenter listPresenter items isEmpty
]

{ #category : 'running' }
MyRunnerPresenterTreeTest >> testANewTestRunnerPresenterShouldntHaveAnyResultDescription [

	self assert: testRunnerUI resultDescriptionPresenter text isEmpty
]

{ #category : 'running' }
MyRunnerPresenterTreeTest >> testANewTestRunnerPresenterShouldntHaveAnySelectedPackage [

	self assert:
		testRunnerUI packagesPresenter listPresenter selectedItem isNil
]

{ #category : 'running' }
MyRunnerPresenterTreeTest >> testClassesPresenterContainsTestCasesFromTheSelectedPackages [

	| expectedNumberOfClasses aPackage |
	aPackage := #'TestRunner-Core' asPackage.
	expectedNumberOfClasses := (aPackage definedClasses select: [ :class | 
		                            class isTestCase ]) size.
	self assert:
		testRunnerUI classesPresenter listPresenter items isEmpty.

	testRunnerUI packagesPresenter listPresenter selectItems:
		{ aPackage }.
	self
		assert: testRunnerUI classesPresenter listPresenter items size
		equals: expectedNumberOfClasses
]

{ #category : 'running' }
MyRunnerPresenterTreeTest >> testInitilyTheResultPresenterItemsShouldBeEmpty [

	self assert: testRunnerUI resultItemsPresenter roots isEmpty
]

{ #category : 'running' }
MyRunnerPresenterTreeTest >> testItemSelectionInPackagePresenter [

	self assert: testRunnerUI packagesPresenter selectedItem isNil.
	testRunnerUI packagesPresenter listPresenter selectItems:
		{ #'TestRunner-Tests' asPackage }.
	self deny: testRunnerUI packagesPresenter selectedItem isNil
]

{ #category : 'running' }
MyRunnerPresenterTreeTest >> testResultDescriptionPresenterShouldContainsTheTestsExecutionStats [

	| aPackage aClass expectedDescription |
	aPackage := #'TestRunner-Tests' asPackage.
	aClass := GreenTest.
	expectedDescription := '2 ran, 2 passed, 0 skipped, 0 expected failures, 0 failures, 0 errors, 0 passed unexpected'.
	testRunnerUI packagesPresenter selectItem: aPackage.
	testRunnerUI classesPresenter selectItem: aClass.
	testRunnerUI executeSelectedClasses.
	self
		assert: testRunnerUI resultDescriptionPresenter text
		equals: expectedDescription
]
