"
An ALSourceCleanerTest is a test class for testing the behavior of ALSourceCleaner
"
Class {
	#name : #ReMethodSourceCleanerTest,
	#superclass : #TestCase,
	#category : #'Renraku-Tests-Utilities'
}

{ #category : #tests }
ReMethodSourceCleanerTest >> testCleaning [

	ReMethodSourceCleaner clean: (ObsoleteTest>>#testClassObsolete) sourceCode
]
