Class {
	#name : #BaseStreamTest,
	#superclass : #TestCase,
	#category : #'Tests-Files'
}

{ #category : #testing }
BaseStreamTest class >> isAbstract [
	^ self = BaseStreamTest.
]

{ #category : #testing }
BaseStreamTest class >> isUnitTest [
	^false
]

{ #category : #accessing }
BaseStreamTest >> baseStreamType [
	"Answer the stream type to be tested"
	self shouldBeImplemented.
]

{ #category : #accessing }
BaseStreamTest >> testFileName [
	^ self class name,'_', testSelector , '_test'
]

{ #category : #testing }
BaseStreamTest >> testFileNamed [
	| testString fileName |
	"Given"
	fileName := self testFileName.
	testString := 'testString'.
	"When"
	[ self baseStreamType fileNamed: fileName do: [ :streamType | streamType nextPut: testString ].
	"Then"
	self baseStreamType fileNamed: fileName do: [ :streamType | self assert: streamType next equals: testString ] ]
		ensure: [ (FileSystem disk workingDirectory / fileName) ensureDelete ]
]

{ #category : #testing }
BaseStreamTest >> testReadOnlyFileNamed [
	| testString fileName file |
	"Given"
	fileName := self testFileName.
	testString := 'testString'.
	"When"
	[ self baseStreamType fileNamed: fileName do: [ :streamType | streamType nextPut: testString ].
	"Then"
	file := self baseStreamType readOnlyFileNamed: fileName.
	self assert: file next equals: testString.
	file close ]
		ensure: [ fileName asFileReference delete ]
]
