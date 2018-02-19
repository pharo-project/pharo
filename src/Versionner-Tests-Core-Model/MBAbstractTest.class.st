"
Common superclass for test for Versionner
"
Class {
	#name : #MBAbstractTest,
	#superclass : #TestCase,
	#instVars : [
		'configuration'
	],
	#category : #'Versionner-Tests-Core-Model'
}

{ #category : #testing }
MBAbstractTest class >> isAbstract [
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"
	
	^ self name = #MBAbstractTest
]

{ #category : #accessing }
MBAbstractTest >> configurationName [

	^'ConfigurationOfDummyConfiguration'
]

{ #category : #util }
MBAbstractTest >> removeClassIfExist: aSymbol [
	Smalltalk globals at: aSymbol ifPresent: [ :cls | cls removeFromSystem ]
]

{ #category : #running }
MBAbstractTest >> setUp [
	super setUp.
	(Smalltalk globals includesKey: self configurationName asSymbol) 
		ifFalse: [ 
			MetacelloToolBox configurationNamed: self configurationName.
				(Smalltalk globals at: self configurationName asSymbol) compile: 'baseline10: spec 
        <version: ''1.0-baseline''>
        
        spec for: #common do: [
                spec blessing: #baseline.
                spec repository: ''http://www.squeaksource.com/Versionner''.
		    spec package: ''Collections-Streams''.
                ].'.

			(Smalltalk globals at: self configurationName asSymbol) compile: 'default: spec 
        <version: ''default''>
        
        spec for: #common do: [
                spec blessing: #baseline.
                spec repository: ''http://www.squeaksource.com/Versionner''.
		    spec package: ''Collections-Streams''.
                ].'.
			].
		
	configuration := (Smalltalk globals at: self configurationName asSymbol) 

]

{ #category : #running }
MBAbstractTest >> tearDown [
	
	configuration := nil.
	self removeClassIfExist: self configurationName asSymbol.
	super tearDown

]
