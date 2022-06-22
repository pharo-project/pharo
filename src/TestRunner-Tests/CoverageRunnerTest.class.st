Class {
	#name : #CoverageRunnerTest,
	#superclass : #TestCase,
	#instVars : [
		'coverageRunner'
	],
	#category : #'TestRunner-Tests-Core'
}

{ #category : #running }
CoverageRunnerTest >> setUp [
	super setUp.
	coverageRunner := CoverageRunner new
]

{ #category : #tests }
CoverageRunnerTest >> testAddingClassToCoverageRunner [

	| expectedMethodsCount |
	coverageRunner addClass: Integer.
	expectedMethodsCount := Integer methods size.
	coverageRunner addClass: TestRunnerTest.
	expectedMethodsCount := expectedMethodsCount
	                        + TestRunnerTest methods size.

	self assert: coverageRunner methods size equals: expectedMethodsCount
]

{ #category : #tests }
CoverageRunnerTest >> testAllMethodsHaveAnnotationAfterPeRunningProcess [

	coverageRunner addClass: Integer.
	coverageRunner addLinkToAllMethodsFromClasses.
	self assert: (coverageRunner methods allSatisfy: [ :m |
			m ast hasMetalink: coverageRunner link ])
]

{ #category : #tests }
CoverageRunnerTest >> testCoverageOn1ClassesAnd2Test [

	coverageRunner addTestCase: MyClassCovered50Test.
	coverageRunner addTestCase: MyClassCovered100Test.

	coverageRunner addClass: MyClassCovered100.

	coverageRunner run.
	self assert: coverageRunner coverage equals: 100
]

{ #category : #tests }
CoverageRunnerTest >> testCoverageOn1ClassesAnd2Test2 [

	coverageRunner addTestCase: MyClassCovered50Test.
	coverageRunner addTestCase: MyClassCovered100Test.

	coverageRunner addClass: MyClassCovered50.

	coverageRunner run.
	self assert: coverageRunner coverage equals: 50
]

{ #category : #tests }
CoverageRunnerTest >> testCoverageOn2ClassesAnd1Test [

	coverageRunner addTestCase: MyClassCovered100Test.

	coverageRunner addClass: MyClassCovered100.
	coverageRunner addClass: MyClassCovered50.

	coverageRunner run.
	self assert: coverageRunner coverage equals: 50
]

{ #category : #tests }
CoverageRunnerTest >> testCoverageOn2ClassesAnd1Test2 [

	coverageRunner addTestCase: MyClassCovered50Test.

	coverageRunner addClass: MyClassCovered100.
	coverageRunner addClass: MyClassCovered50.

	coverageRunner run.
	self assert: coverageRunner coverage equals: 25
]

{ #category : #tests }
CoverageRunnerTest >> testCoverageOn2ClassesAnd2Tests [

	coverageRunner addTestCase: MyClassCovered100Test.
	coverageRunner addTestCase: MyClassCovered50Test.

	coverageRunner addClass: MyClassCovered100.
	coverageRunner addClass: MyClassCovered50.

	coverageRunner run.
	self assert: coverageRunner coverage equals: 75
]

{ #category : #tests }
CoverageRunnerTest >> testLinksAreRemovedFromMethodAtTheEndOfPostExecution [

	coverageRunner addClass: Integer.
	coverageRunner run.
	self assert: (coverageRunner methods allSatisfy: [ :m | 
			 (m ast hasMetalink: coverageRunner link) not ])
]

{ #category : #tests }
CoverageRunnerTest >> testNewCoverageRunnerHasAnyTestToRun [

	self deny: coverageRunner hasClassToRun
]

{ #category : #tests }
CoverageRunnerTest >> testNewCreatedCoverageRunnerCoverageIsNil [

	self should: [ coverageRunner coverage equals: 0 ] raise: Error
]

{ #category : #tests }
CoverageRunnerTest >> testNewCreatedMethodsDoesNotHaveAnnotation [

	coverageRunner addClass: Integer.
	coverageRunner initializeMethods.
	self assert: (coverageRunner methods allSatisfy: [ :m | 
			 (m ast hasMetalink: coverageRunner link) not ])
]

{ #category : #tests }
CoverageRunnerTest >> testOnClassCoveredHalf [

	coverageRunner addTestCase: MyClassCovered50Test.
	coverageRunner addClass: MyClassCovered50.
	
	coverageRunner run.
	
	self assert: coverageRunner coverage equals: 50
]

{ #category : #tests }
CoverageRunnerTest >> testOnClassFullyCovered [

	coverageRunner addTestCase: MyClassCovered100Test.
	
	coverageRunner addClass: MyClassCovered100.
	
	coverageRunner run.
	
	self assert: coverageRunner coverage equals: 100
]

{ #category : #tests }
CoverageRunnerTest >> testOnNotCoveredClass [

	coverageRunner addTestCase: MyClassCovered100Test.
	
	coverageRunner addClass: MyClassCovered50.
	
	coverageRunner run.
	
	self assert: coverageRunner coverage equals: 0
]

{ #category : #tests }
CoverageRunnerTest >> testOnNotCoveredClass2 [

	coverageRunner addTestCase: MyClassCovered50Test.
	coverageRunner addClass: MyClassCovered100.
	coverageRunner run.
	self assert: coverageRunner coverage equals: 0
]
