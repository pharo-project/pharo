"
Pharo code should not contain linefeed characters.
"
Class {
	#name : 'ReMethodSourceContainsLinefeedsRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Bugs',
	#package : 'General-Rules',
	#tag : 'Bugs'
}

{ #category : 'testing' }
ReMethodSourceContainsLinefeedsRule class >> checksMethod [
	^ true
]

{ #category : 'accessing' }
ReMethodSourceContainsLinefeedsRule class >> group [
	^ 'Bugs'
]

{ #category : 'accessing' }
ReMethodSourceContainsLinefeedsRule class >> ruleName [
	^ 'Method source contains linefeeds'
]

{ #category : 'accessing' }
ReMethodSourceContainsLinefeedsRule class >> severity [
	^ #error
]

{ #category : 'accessing' }
ReMethodSourceContainsLinefeedsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'MethodSourceContainsLinefeedsRule'
]

{ #category : 'running' }
ReMethodSourceContainsLinefeedsRule >> check: aMethod forCritiquesDo: aCriticBlock [
	| linefeeds |
	linefeeds := aMethod sourceCode allRangesOfSubstring: String lf.

	linefeeds do: [ :lfInterval |
			aCriticBlock cull: (ReTrivialCritique
			withAnchor: (ReIntervalSourceAnchor
				entity: aMethod
				interval: lfInterval)
			by: self
			hint: 'lf') ]
]
