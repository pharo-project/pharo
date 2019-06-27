Class {
	#name : #ClyRegexPatternTest,
	#superclass : #ClyStringPatternTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #tests }
ClyRegexPatternTest >> testMatchesAnyStringWhenItIsBadRegex [

	| pattern |
	pattern := ClyRegexPattern with: '!*'.
	
	self assert: (pattern matches: 'any string')
]

{ #category : #tests }
ClyRegexPatternTest >> testMatchesAnyStringWhenItIsEmpty [

	| pattern |
	pattern := ClyRegexPattern with: ''.
	
	self assert: (pattern matches: 'any string')
]

{ #category : #tests }
ClyRegexPatternTest >> testMatchesAnyStringWhenItIsOnlySpaces [

	| pattern |
	pattern := ClyRegexPattern with: '  '.
	
	self assert: (pattern matches: 'any string')
]

{ #category : #tests }
ClyRegexPatternTest >> testMatchesStringWhichMatchesCorrectRegex [

	| pattern |
	pattern := ClyRegexPattern with: '.abs'.
	
	self assert: (pattern matches: '1abs').
	self deny: (pattern matches: 'abs')
]

{ #category : #tests }
ClyRegexPatternTest >> testTriesFixWronglyUsedStarInRegex [

	| pattern |
	pattern := ClyRegexPattern with: '*abs'.
	
	self assert: (pattern matches: 'abs').
	self deny: (pattern matches: 'ert')
]
