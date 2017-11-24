"
Utility class
"
Class {
	#name : #MCFrontier,
	#superclass : #Object,
	#instVars : [
		'frontier',
		'bag'
	],
	#category : #'Monticello-Versioning'
}

{ #category : #'instance creation' }
MCFrontier class >> frontierOn: aVersionInfo [
	^ self frontierOnAll: (Array with: aVersionInfo)
]

{ #category : #'instance creation' }
MCFrontier class >> frontierOn: aVersionInfo and: otherVersionInfo [
	^ self frontierOnAll: (Array with: aVersionInfo with: otherVersionInfo)
]

{ #category : #'instance creation' }
MCFrontier class >> frontierOnAll: aCollection [
	| remaining  allVersions |
	remaining := Bag new.
	allVersions := (aCollection flatCollect: [:ea | ea withBreadthFirstAncestors]) asSet.
	allVersions do: [:ea | remaining addAll: ea ancestors].
	^self new frontier: aCollection bag: remaining
]

{ #category : #accessing }
MCFrontier >> frontier [
	^frontier
]

{ #category : #initialization }
MCFrontier >> frontier: f bag: remaining [
	frontier := f asOrderedCollection.
	bag := remaining
]

{ #category : #advancing }
MCFrontier >> remove: aVersionInfo [
	frontier remove: aVersionInfo.
	aVersionInfo ancestors  do:
		[ :ancestor |
			bag remove: ancestor.
			(bag occurrencesOf: ancestor) = 0
				ifTrue: [frontier add: ancestor]].
	^aVersionInfo
]

{ #category : #advancing }
MCFrontier >> removeAll: collection [
	collection do: [ :n | self remove: n]
]
