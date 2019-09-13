"
I am CacheStatistics.
I keep track of hits and misses of a cache.
"
Class {
	#name : #CacheStatistics,
	#superclass : #Object,
	#instVars : [
		'hits',
		'misses'
	],
	#category : #'System-Caching'
}

{ #category : #accessing }
CacheStatistics >> addHit [
	hits := hits + 1
]

{ #category : #accessing }
CacheStatistics >> addMiss [
	misses := misses + 1
]

{ #category : #accessing }
CacheStatistics >> hitRatio [
	"Return the ratio of hits against total calls I received.
	This will be a number between 0 and 1.
	When I am empty, return 0."
	
	| total |
	total := hits + misses.
	total = 0 ifTrue: [ ^ 0 ].
	^ hits / total
]

{ #category : #accessing }
CacheStatistics >> hits [
	^ hits
]

{ #category : #initialization }
CacheStatistics >> initialize [
	super initialize.
	hits := misses := 0
]

{ #category : #accessing }
CacheStatistics >> misses [
	^ misses
]

{ #category : #initialization }
CacheStatistics >> reset [
	hits := misses := 0
]
