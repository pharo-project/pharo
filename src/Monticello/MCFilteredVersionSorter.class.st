"
Sort filtered versions 
"
Class {
	#name : #MCFilteredVersionSorter,
	#superclass : #MCVersionSorter,
	#instVars : [
		'target'
	],
	#category : 'Monticello-Versioning'
}

{ #category : #adding }
MCFilteredVersionSorter >> addVersionInfo: aVersionInfo [
	(aVersionInfo hasAncestor: target)
		ifTrue: [super addVersionInfo: aVersionInfo]

]

{ #category : #private }
MCFilteredVersionSorter >> processVersionInfo: aVersionInfo [
	| success |
	aVersionInfo = target ifTrue: [^ true].
	(aVersionInfo hasAncestor: target) ifFalse: [^false].
	self pushLayer.
	success := (self knownAncestorsOf: aVersionInfo) anySatisfy:
				[:ea | self processVersionInfo: ea].
	self popLayer.
	success ifTrue: [self addToCurrentLayer: aVersionInfo].
	^ success	
]

{ #category : #accessing }
MCFilteredVersionSorter >> target: aVersionInfo [
	target := aVersionInfo
]
