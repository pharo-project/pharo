"
Abstract superclass of records of ancestry.
"
Class {
	#name : #MCAncestry,
	#superclass : #Object,
	#instVars : [
		'ancestors',
		'stepChildren'
	],
	#category : #Monticello-Versioning
}

{ #category : #ancestry }
MCAncestry >> allAncestorsDo: aBlock [
	self ancestors do:
		[:ea |
		aBlock value: ea.
		ea allAncestorsDo: aBlock]
]

{ #category : #ancestry }
MCAncestry >> allAncestorsOnPathTo: aVersionInfo [
	^ MCFilteredVersionSorter new
		target: aVersionInfo;
		addAllVersionInfos: self ancestors;
		sortedVersionInfos
]

{ #category : #ancestry }
MCAncestry >> ancestorString [
	^ String streamContents:
		[:s | self ancestors do: [:ea | s nextPutAll: ea name] separatedBy: [s nextPutAll: ', ']]
]

{ #category : #ancestry }
MCAncestry >> ancestorStringWithout: packageName [
	^ String streamContents:
		[:s | self ancestors do: [:ea | s nextPutAll: (ea nameWithout: packageName)] separatedBy: [s nextPutAll: ', ']]
]

{ #category : #ancestry }
MCAncestry >> ancestors [
	^ ancestors ifNil: [#()]
]

{ #category : #ancestry }
MCAncestry >> ancestorsDoWhileTrue: aBlock [
	self ancestors do:
		[:ea |
		(aBlock value: ea) ifTrue: 
			[ea ancestorsDoWhileTrue: aBlock]]
]

{ #category : #ancestry }
MCAncestry >> breadthFirstAncestors [
	^ Array streamContents: [:s | self breadthFirstAncestorsDo: [:ea | s nextPut: ea]]
]

{ #category : #ancestry }
MCAncestry >> breadthFirstAncestorsDo: aBlock [
	| seen todo next |
	seen := Set with: self.
	todo := OrderedCollection with: self.
	[todo isEmpty] whileFalse:
		[next := todo removeFirst.
		next ancestors do:
			[:ea |
			(seen includes: ea) ifFalse:
				[aBlock value: ea.
				seen add: ea.
				todo add: ea]]]
]

{ #category : #ancestry }
MCAncestry >> commonAncestorWith: aNode [
	| commonAncestors |
	commonAncestors := self commonAncestorsWith: aNode.
	^ commonAncestors at: 1 ifAbsent: [nil]
]

{ #category : #ancestry }
MCAncestry >> commonAncestorsWith: aVersionInfo [

	| sharedAncestors mergedOrder sorter |
	sorter := MCVersionSorter new
						addVersionInfo: self;
						addVersionInfo: aVersionInfo.
	mergedOrder := sorter sortedVersionInfos.
	sharedAncestors := (sorter allAncestorsOf: self) intersection: (sorter allAncestorsOf: aVersionInfo).
	^ mergedOrder select: [:ea | sharedAncestors includes: ea]
]

{ #category : #ancestry }
MCAncestry >> hasAncestor: aVersionInfo [
	^ self
		hasAncestor: aVersionInfo
		alreadySeen: OrderedCollection new
]

{ #category : #ancestry }
MCAncestry >> hasAncestor: aVersionInfo alreadySeen: aList [
	(aList includes: self) ifTrue: [^ false].
	aList add: self.

	^ self = aVersionInfo or: [self ancestors anySatisfy: [:ea | ea hasAncestor: aVersionInfo alreadySeen: aList]]

]

{ #category : #initialization }
MCAncestry >> initialize [
	super initialize.
	ancestors := #().
	stepChildren := #()
]

{ #category : #ancestry }
MCAncestry >> isRelatedTo: aVersionInfo [
	^ aVersionInfo timeStamp < self timeStamp
		ifTrue: [self hasAncestor: aVersionInfo]
		ifFalse: [aVersionInfo hasAncestor: self]
]

{ #category : #ancestry }
MCAncestry >> stepChildren [
	^ stepChildren ifNil: [#()]
]

{ #category : #ancestry }
MCAncestry >> stepChildrenString [
	^ String streamContents:
		[:s | self stepChildren do: [:ea | s nextPutAll: ea name] separatedBy: [s nextPutAll: ', ']]
]

{ #category : #ancestry }
MCAncestry >> topologicalAncestors [
	
	^ Array streamContents:
		[:s | | f frontier |
		frontier := MCFrontier frontierOn: self.
		[f := frontier frontier.
		s nextPutAll: f.
		frontier removeAll: f.
		f isEmpty] whileFalse] 
]

{ #category : #ancestry }
MCAncestry >> withBreadthFirstAncestors [
	^ (Array with: self), self breadthFirstAncestors
]
