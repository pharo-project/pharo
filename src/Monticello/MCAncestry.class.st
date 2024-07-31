"
Abstract superclass of records of ancestry.
"
Class {
	#name : 'MCAncestry',
	#superclass : 'Object',
	#instVars : [
		'ancestors',
		'stepChildren'
	],
	#category : 'Monticello-Versioning',
	#package : 'Monticello',
	#tag : 'Versioning'
}

{ #category : 'ancestry' }
MCAncestry >> ancestors [
	^ ancestors ifNil: [#()]
]

{ #category : 'ancestry' }
MCAncestry >> commonAncestorWith: aNode [

	^ (self commonAncestorsWith: aNode) at: 1 ifAbsent: [ nil ]
]

{ #category : 'ancestry' }
MCAncestry >> commonAncestorsWith: aVersionInfo [

	| sharedAncestors mergedOrder sorter |
	sorter := MCVersionSorter new
						addVersionInfo: self;
						addVersionInfo: aVersionInfo.
	mergedOrder := sorter sortedVersionInfos.
	sharedAncestors := (sorter allAncestorsOf: self) intersection: (sorter allAncestorsOf: aVersionInfo).
	^ mergedOrder select: [:ea | sharedAncestors includes: ea]
]

{ #category : 'ancestry' }
MCAncestry >> hasAncestor: aVersionInfo [
	^ self
		hasAncestor: aVersionInfo
		alreadySeen: OrderedCollection new
]

{ #category : 'ancestry' }
MCAncestry >> hasAncestor: aVersionInfo alreadySeen: aList [
	(aList includes: self) ifTrue: [^ false].
	aList add: self.

	^ self = aVersionInfo or: [self ancestors anySatisfy: [:ea | ea hasAncestor: aVersionInfo alreadySeen: aList]]

]

{ #category : 'initialization' }
MCAncestry >> initialize [
	super initialize.
	ancestors := #().
	stepChildren := #()
]

{ #category : 'ancestry' }
MCAncestry >> stepChildren [
	^ stepChildren ifNil: [#()]
]
