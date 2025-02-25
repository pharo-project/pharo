Class {
	#name : 'MCDataStreamTest',
	#superclass : 'TestCase',
	#category : 'Monticello-Tests-IO',
	#package : 'Monticello-Tests',
	#tag : 'IO'
}

{ #category : 'accessing' }
MCDataStreamTest >> testFileName [
	^ self class name,'_', testSelector , '_test'
]

{ #category : 'testing' }
MCDataStreamTest >> testFileNamed [
	| testString fileName |
	"Given"
	fileName := self testFileName.
	testString := 'testString'.
	"When"
	[  MCDataStream fileNamed: fileName do: [ :streamType | streamType nextPut: testString ].
	"Then"
	 MCDataStream readOnlyFileNamed: fileName do: [ :streamType | self assert: streamType next equals: testString ] ]
		ensure: [ (FileSystem disk workingDirectory / fileName) ensureDelete ]
]

{ #category : 'testing' }
MCDataStreamTest >> testReadOnlyFileNamed [
	| testString fileName file |
	"Given"
	fileName := self testFileName.
	testString := 'testString'.
	"When"
	[ MCDataStream fileNamed: fileName do: [ :streamType | streamType nextPut: testString ].
	"Then"
	file := MCDataStream readOnlyFileNamed: fileName.
	self assert: file next equals: testString.
	file close ]
		ensure: [ fileName asFileReference delete ]
]
