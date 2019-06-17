Class {
	#name : #ClySubstringPatternTest,
	#superclass : #ClyStringPatternTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #tests }
ClySubstringPatternTest >> testCaseInsensitiveByDefault [

	| pattern |
	pattern := ClySubstringPattern with: 'subString'.
	
	self deny: pattern isCaseSensitive
]

{ #category : #tests }
ClySubstringPatternTest >> testCaseInsensitiveMathingString [

	| pattern |
	pattern := ClySubstringPattern with: 'subString' caseSensitive: false.
	
	self assert: (pattern matches: 'start subString end').
	self assert: (pattern matches: 'start substring end').
	
	self deny: (pattern matches: 'another string').
]

{ #category : #tests }
ClySubstringPatternTest >> testCaseSensitiveMathingString [

	| pattern |
	pattern := ClySubstringPattern with: 'subString' caseSensitive: true.
	
	self assert: (pattern matches: 'start subString end').
	
	self deny: (pattern matches: 'start substring end').
]

{ #category : #tests }
ClySubstringPatternTest >> testComparison [

	| pattern pattern2 |
	pattern := ClySubstringPattern with: 'subString' caseSensitive: true.
	pattern2 := ClySubstringPattern with: 'subString' copy caseSensitive: true.
	self assert: pattern equals: pattern2.
	self assert: pattern hash equals: pattern2 hash.
	
	pattern2 value: 'another pattern'.
	self deny: pattern = pattern2.
	
	pattern2 value: pattern value.
	pattern2 isCaseSensitive: pattern isCaseSensitive not.
	self deny: pattern = pattern2
]
