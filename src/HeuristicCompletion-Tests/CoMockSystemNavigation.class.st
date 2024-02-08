"
I am a mock environment. The tests use me to scope the environment to a known one.
"
Class {
	#name : 'CoMockSystemNavigation',
	#superclass : 'Object',
	#instVars : [
		'globals',
		'selectors'
	],
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'query' }
CoMockSystemNavigation >> allGlobalNamesStartingWith: aString do: aBlock caseSensitive: cs [

	globals do: [ :e |
		(aString isEmpty or: [ e beginsWith: aString caseSensitive: cs ])
			ifTrue: [ aBlock value: e ] ]
]

{ #category : 'accessing' }
CoMockSystemNavigation >> allSelectorsStartingWith: aString do: aBlock [

	selectors do: [ :e |
		( e beginsWith: aString )
			ifTrue: [ aBlock value: e ] ]
]

{ #category : 'accessing' }
CoMockSystemNavigation >> environment [

	^ globals
]

{ #category : 'accessing' }
CoMockSystemNavigation >> globals [
	^ globals
]

{ #category : 'accessing' }
CoMockSystemNavigation >> globals: anObject [
	globals := anObject
]

{ #category : 'accessing' }
CoMockSystemNavigation >> selectors [

	^ selectors
]

{ #category : 'accessing' }
CoMockSystemNavigation >> selectors: anObject [

	selectors := anObject
]
