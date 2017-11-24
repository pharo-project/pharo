"
SUnit tests for MBSpecInfo in Versionner
"
Class {
	#name : #MBSpecInfoTest,
	#superclass : #MBAbstractTest,
	#category : #'Versionner-Tests-Core-Model'
}

{ #category : #testing }
MBSpecInfoTest class >> isAbstract [
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"
	
	^ self name = #MBSpecInfoTest
]
