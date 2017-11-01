"
A trait for test purposes
"
Trait {
	#name : #TCopySequenceableSameContents,
	#category : 'Collections-Tests-Abstract'
}

{ #category : #requirements }
TCopySequenceableSameContents >> collectionWithSortableElements [
	" return a collection only including elements that can be sorted (understanding '<' )"
	self explicitRequirement
]

{ #category : #requirements }
TCopySequenceableSameContents >> empty [
	self explicitRequirement
]

{ #category : #requirements }
TCopySequenceableSameContents >> nonEmpty [
	self explicitRequirement
]

{ #category : #'tests - fixture' }
TCopySequenceableSameContents >> test0FixtureCopySameContentsTest [
	self nonEmpty.
	self deny: self nonEmpty isEmpty.
	self empty.
	self assert: self empty isEmpty
]

{ #category : #'tests - copying same contents' }
TCopySequenceableSameContents >> testReverse [

	| result |
	result := self nonEmpty reversed.	
	"verify content of 'result: '"
	1 to: result size do:
		[:i | self assert: ((result at: i) 
			= (self nonEmpty at: (self nonEmpty size - i + 1)))].
	"verify size of 'result' :"
	self assert: result size=self nonEmpty size.
]

{ #category : #'tests - copying same contents' }
TCopySequenceableSameContents >> testReversed [
	| result |
	result := self nonEmpty reversed .
	
	"verify content of 'result: '"
	1 to:  result size do:
		[:i | self assert: ((result at:i)=(self nonEmpty at:(self nonEmpty size-i+1)))].
	"verify size of 'result' :"
	self assert: result size=self nonEmpty size.
]

{ #category : #'tests - copying same contents' }
TCopySequenceableSameContents >> testShallowCopy [
	| result |
	result := self nonEmpty shallowCopy .
	
	"verify content of 'result: '"
	1 to: self nonEmpty size do:
		[:i | self assert: ((result at:i)=(self nonEmpty at:i))].
	"verify size of 'result' :"
	self assert: result size=self nonEmpty size.
]

{ #category : #'tests - copying same contents' }
TCopySequenceableSameContents >> testShallowCopyEmpty [
	| result |
	result := self empty shallowCopy .
	self assert: result isEmpty .
]

{ #category : #'tests - copying same contents' }
TCopySequenceableSameContents >> testShuffled [
	| result |
	result := self nonEmpty shuffled .
	
	"verify content of 'result: '"
	result do: [:each | self assert: (self nonEmpty occurrencesOf: each)=(result occurrencesOf: each)].
	"verify size of 'result' :"
	self assert: result size=self nonEmpty size.
]
