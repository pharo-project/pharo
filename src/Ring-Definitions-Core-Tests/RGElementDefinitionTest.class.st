Class {
	#name : 'RGElementDefinitionTest',
	#superclass : 'TestCase',
	#category : 'Ring-Definitions-Core-Tests-Base',
	#package : 'Ring-Definitions-Core-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
RGElementDefinitionTest >> testParseTimestampFromDefault [

	| default |
	self
		assert: (RGElementDefinition parseTimestampFrom: '2025-04-25T14:50:52.000000+02:00')
		equals: (DateAndTime year: 2025 month: 4 day: 25 hour: 14 minute: 50 second: 52 nanoSecond: 0 offset: 2 hours).
	default := Object new.
	self assert: (RGElementDefinition parseTimestampFrom: 'bogus' default: default) identicalTo: default
]
