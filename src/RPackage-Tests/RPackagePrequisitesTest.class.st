"
SUnit tests for RPackage prerequisites
"
Class {
	#name : #RPackagePrequisitesTest,
	#superclass : #TestCase,
	#category : #'RPackage-Tests'
}

{ #category : #tests }
RPackagePrequisitesTest >> testNoPackagesOverride [
"Class side packages should not be overridden"
	self assert: (Object allSubclasses select: [: each | (each class compiledMethodAt: #packages ifAbsent: [nil]) isNotNil]) isEmpty.

]
