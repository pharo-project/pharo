"
I know the items to create a testSuite that will be analysed by a DrTestsPlugin.
"
Class {
	#name : #DTTestCoverageConfiguration,
	#superclass : #DTPluginConfiguration,
	#category : #'DrTests-TestCoverage'
}

{ #category : #tests }
DTTestCoverageConfiguration >> testMethods [
	^ (packagesSelected
		flatCollect:
			[ :p | p definedClasses select: [ :c | c allSuperclasses includes: TestCase ] ])
		flatCollect: 
		[ :c | c methods select:[:m| m isTestMethod ] ]
]
