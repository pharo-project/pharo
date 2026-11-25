Class {
	#name : 'ClyRegexPatternTest',
	#superclass : 'ClyStringPatternTest',
	#category : 'Calypso-NavigationModel-Tests',
	#package : 'Calypso-NavigationModel-Tests'
}

{ #category : 'tests' }
ClyRegexPatternTest >> testEscapedLineEndings [ 

	self assert: (
		((ClyRegexPattern with: '^bb\ncc') matches: 
				(String streamContents: [ :s | #( 'aa' 'bb' 'cc') do: [ :e | s << e; lf] ]))
			&
		((ClyRegexPattern with: '^bb\rcc') matches: 
				(String streamContents: [ :s | #( 'aa' 'bb' 'cc') do: [ :e | s << e; cr] ]))
			&
		((ClyRegexPattern with: '^bb\r\ncc') matches: 
			(String streamContents: [ :s | #( 'aa' 'bb' 'cc') do: [ :e | s << e; cr; lf] ]))
	)

]

{ #category : 'tests' }
ClyRegexPatternTest >> testLineEndings [ 

	self assert: ((ClyRegexPattern with: '^b') matches: 
		(String streamContents: [ :s | #( 'aa' 'bb' 'cc') do: [ :e | s << e; cr] ])).
	self assert: ((ClyRegexPattern with: '^b') matches: 
		(String streamContents: [ :s | #( 'aa' 'bb' 'cc') do: [ :e | s << e; lf] ])).
	self assert: ((ClyRegexPattern with: '^b') matches: 
		(String streamContents: [ :s | #( 'aa' 'bb' 'cc') do: [ :e | s << e; cr; lf] ])).

]

{ #category : 'tests' }
ClyRegexPatternTest >> testMatchesAnyStringWhenItIsBadRegex [

	| pattern |
	pattern := ClyRegexPattern with: '!*'.

	self assert: (pattern matches: 'any string')
]

{ #category : 'tests' }
ClyRegexPatternTest >> testMatchesAnyStringWhenItIsEmpty [

	| pattern |
	pattern := ClyRegexPattern with: ''.

	self assert: (pattern matches: 'any string')
]

{ #category : 'tests' }
ClyRegexPatternTest >> testMatchesAnyStringWhenItIsOnlySpaces [

	| pattern |
	pattern := ClyRegexPattern with: '  '.

	self assert: (pattern matches: 'any string')
]

{ #category : 'tests' }
ClyRegexPatternTest >> testMatchesStringWhichMatchesCorrectRegex [

	| pattern |
	pattern := ClyRegexPattern with: '.abs'.

	self assert: (pattern matches: '1abs').
	self deny: (pattern matches: 'abs')
]

{ #category : 'tests' }
ClyRegexPatternTest >> testTriesFixWronglyUsedStarInRegex [

	| pattern |
	pattern := ClyRegexPattern with: '*abs'.

	self assert: (pattern matches: 'abs').
	self deny: (pattern matches: 'ert')
]
