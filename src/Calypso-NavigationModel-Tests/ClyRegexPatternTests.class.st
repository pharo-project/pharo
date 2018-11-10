Class {
	#name : #ClyRegexPatternTests,
	#superclass : #ClyStringPatternTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #tests }
ClyRegexPatternTests >> testMatchesAnyStringWhenItIsBadRegex [

	| pattern |
	pattern := ClyRegexPattern with: '!*'.
	
	self assert: (pattern matches: 'any string')
]

{ #category : #tests }
ClyRegexPatternTests >> testMatchesAnyStringWhenItIsEmpty [

	| pattern |
	pattern := ClyRegexPattern with: ''.
	
	self assert: (pattern matches: 'any string')
]

{ #category : #tests }
ClyRegexPatternTests >> testMatchesAnyStringWhenItIsOnlySpaces [

	| pattern |
	pattern := ClyRegexPattern with: '  '.
	
	self assert: (pattern matches: 'any string')
]

{ #category : #tests }
ClyRegexPatternTests >> testMatchesStringWhichMatchesCorrectRegex [

	| pattern |
	pattern := ClyRegexPattern with: '.abs'.
	
	self assert: (pattern matches: '1abs').
	self deny: (pattern matches: 'abs')
]

{ #category : #tests }
ClyRegexPatternTests >> testTriesFixWronglyUsedStarInRegex [

	| pattern |
	pattern := ClyRegexPattern with: '*abs'.
	
	self assert: (pattern matches: 'abs').
	self deny: (pattern matches: 'ert')
]
