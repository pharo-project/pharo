Class {
	#name : #TonelRepositoryTest,
	#superclass : #MCDirectoryRepositoryTest,
	#category : #'MonticelloTonel-Tests'
}

{ #category : #'private factory' }
TonelRepositoryTest >> newMemoryFileSystem [
	"This method assumes writer works. If it is broken (the tests should show it), this tests 
	 will break too."
	| mem |
	mem := FileSystem memory root.
	(TonelWriter on: mem) writeSnapshot: self mockSnapshot.
	^ mem
]

{ #category : #running }
TonelRepositoryTest >> setUp [
	super setUp.
	repository := TonelRepository new directory: self directory

]

{ #category : #tests }
TonelRepositoryTest >> testCanCreateTonelRepositoryFromUrl [
	repository := MCRepository fromUrl: (self homeDirectoryUrl scheme: #tonel) asString.
	self assert: (repository isKindOf: TonelRepository).
	
	repository := MCRepository fromUrl: 'tonel://.'.
	self assert: (repository isKindOf: TonelRepository) 
]

{ #category : #tests }
TonelRepositoryTest >> testLoadAllFileNames [
	| mem |
	
	mem := self newMemoryFileSystem.
	repository := TonelRepository new directory: mem.
	
	self 
		assert: repository allFileNames 
		equals: #('MonticelloMocks')
		
]
