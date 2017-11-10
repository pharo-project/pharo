Class {
	#name : #NECStringSortingTest,
	#superclass : #TestCase,
	#category : #NECompletion-Tests
}

{ #category : #tests }
NECStringSortingTest >> testCaseSensitiveMap [
	| map colon o |
	map := String classPool at: #CaseSensitiveOrder.
	colon := map at: $: asciiValue + 1.
	o := map at: $O asciiValue + 1.
	self assert: colon > o
]

{ #category : #tests }
NECStringSortingTest >> testCompare [
	self assert: ('at:' compare: 'atOne' caseSensitive: false)
			== 1
]

{ #category : #tests }
NECStringSortingTest >> testCompareWithCase [
	self assert: ('at:' compare: 'atOne' caseSensitive: true)
			== 3
]
