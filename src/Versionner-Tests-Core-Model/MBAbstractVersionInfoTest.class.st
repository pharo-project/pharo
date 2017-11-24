"
Abstract superclass for version info tests for Versionner
"
Class {
	#name : #MBAbstractVersionInfoTest,
	#superclass : #MBInfoTest,
	#category : #'Versionner-Tests-Core-Model'
}

{ #category : #testing }
MBAbstractVersionInfoTest class >> isAbstract [
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"
	
	^ self name = #MBAbstractVersionInfoTest
]
