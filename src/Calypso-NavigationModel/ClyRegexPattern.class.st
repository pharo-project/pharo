"
I represent string pattern using regex expression
"
Class {
	#name : #ClyRegexPattern,
	#superclass : #ClyStringPattern,
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #testing }
ClyRegexPattern >> createFixedRegexMatcherIfFailed: errorBlock [

	| fixedPattern |
	fixedPattern := value copyReplaceAll: '*' with: '.*' .
	
	^ [ fixedPattern asRegexIgnoringCase ]
		on: RegexSyntaxError
		do: [ :ex | errorBlock value ]
]

{ #category : #testing }
ClyRegexPattern >> createRegexMatcherIfFailed: errorBlock [

	value trimBoth ifEmpty: [^errorBlock value].
	
	^ [ value asRegexIgnoringCase ]
		on: RegexSyntaxError
		do: [ :ex | ^self createFixedRegexMatcherIfFailed: errorBlock ]
]

{ #category : #testing }
ClyRegexPattern >> matches: aString [
	| matcher |
	matcher := self createRegexMatcherIfFailed: [^true].
	
	^matcher search: aString
]
