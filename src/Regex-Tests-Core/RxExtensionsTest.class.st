Class {
	#name : #RxExtensionsTest,
	#superclass : #TestCase,
	#category : #'Regex-Tests-Core'
}

{ #category : #tests }
RxExtensionsTest >> testSplitJoinBoundaryCases [
	"Empty splitter, joiner or sequence."
	self assert: ('' join: ('.' asRegex split: '')) equals: ''. "NB: Doesn't work with empty regex"
]

{ #category : #tests }
RxExtensionsTest >> testSplitStringOnRegex [
	self assert: ('foobar' splitOn: '[aeiou]+' asRegex)
		equals: #('f' 'b' 'r') asOrderedCollection
]
