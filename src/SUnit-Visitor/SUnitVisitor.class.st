"
I  am an abstract visitor for SUnit.

At my level I  just define the API  a SUnit visitor should implement and some parts of the visit that is likely to be common for my concrete subclasses.

My subclasses should at least override #visitTestCase:. See SunitTestsCounter for a simple concrete example
"
Class {
	#name : #SUnitVisitor,
	#superclass : #Object,
	#category : #'SUnit-Visitor'
}

{ #category : #visiting }
SUnitVisitor >> visit: anObject [
	^ anObject acceptSUnitVisitor: self
]

{ #category : #visiting }
SUnitVisitor >> visitRPackage: aRPackage [
	"Defines the behaviour while visiting a RPackage.
	 The default implementation below is to visit all TestCase subclasses in the package.
	"
	^ aRPackage definedClasses
		select: [ :class | (class isTestCase) and: [ class isAbstract not ] ]
		thenCollect: [ :class | class acceptSUnitVisitor: self ]
]

{ #category : #visiting }
SUnitVisitor >> visitRPackageSet: aRRPackageSet [
	"Defines the behaviour while visiting a RPackageSet.
	 The default implementation below is to visit all RPackages contained in the package set.
	"
	^ aRRPackageSet packages
		collect: [ :package | package acceptSUnitVisitor: self ]
]

{ #category : #visiting }
SUnitVisitor >> visitTestCase: aTestCase [
	"Defines the behaviour while visiting a TestCase.
	 This method must be overriden by concrete subclasses.
	"
	^ self subclassResponsibility
]

{ #category : #visiting }
SUnitVisitor >> visitTestCaseClass: aTestCaseClass [
	"Defines the behaviour while visiting a TestCase class.
	 The default implementation below is to build the suite from local test selectors and visit the suite built.
	"
	^ aTestCaseClass suite
		acceptSUnitVisitor: self
]

{ #category : #visiting }
SUnitVisitor >> visitTestSuite: aTestSuite [
	"Defines the behaviour while visiting a TestSuite.
	 The default implementation below is to iterate on the objects in #tests and return the results of the visit.
	"
	^ aTestSuite tests collect: [ :test |
		test acceptSUnitVisitor: self ]
]
