"
An OmSessionStoreNameStrategyTest is a test class for testing the behavior of OmSessionStoreNameStrategy
"
Class {
	#name : #OmSessionStoreNameStrategyTest,
	#superclass : #TestCase,
	#instVars : [
		'strategy',
		'name'
	],
	#category : #'Ombu-Tests'
}

{ #category : #testing }
OmSessionStoreNameStrategyTest class >> isAbstract [

	^ self name = #OmSessionStoreNameStrategyTest
]

{ #category : #private }
OmSessionStoreNameStrategyTest >> createFileWith: aName [

	self directory ensureCreateDirectory.
	(OmFileStore fileReferenceForStoreNamed: aName inDirectory: self directory) 
		writeStreamDo: [ :stream | stream print: 'stub contents' ]
]

{ #category : #private }
OmSessionStoreNameStrategyTest >> directory [

	^ FileLocator temp asFileReference / 'ombu-strategy-tests'
]

{ #category : #private }
OmSessionStoreNameStrategyTest >> fileReferenceWith: aName [

	^ OmFileStore 
		fileReferenceForStoreNamed: aName
		inDirectory: self directory
]

{ #category : #running }
OmSessionStoreNameStrategyTest >> setUp [

	super setUp.
	strategy := self strategyClass new
]

{ #category : #initialization }
OmSessionStoreNameStrategyTest >> strategyClass [

	^ self class environment classNamed: 
		(self class name allButLast: 4) "corresponding to the word Test"
]

{ #category : #running }
OmSessionStoreNameStrategyTest >> tearDown [

	self directory ensureDeleteAll.
	super tearDown
]

{ #category : #tests }
OmSessionStoreNameStrategyTest >> testWithExistingFile [
	self createFileWith: 'existing'.

	name := strategy nextTo: 'existing' in: self directory.

	self assert: name isString.
	self deny: name equals: 'existing'.
	self deny: (self fileReferenceWith: name) exists
]

{ #category : #tests }
OmSessionStoreNameStrategyTest >> testWithNotExistingFile [

	name := strategy nextTo: 'inexistent' in: self directory.
	
	self assert: name isString.
	self deny: (self fileReferenceWith: name) exists
]
