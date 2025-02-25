"
Sort versions
"
Class {
	#name : 'MCVersionSorter',
	#superclass : 'Object',
	#instVars : [
		'layers',
		'depthIndex',
		'depths',
		'stepparents',
		'roots'
	],
	#category : 'Monticello-Versioning',
	#package : 'Monticello',
	#tag : 'Versioning'
}

{ #category : 'adding' }
MCVersionSorter >> addAllAncestorsOf: aVersionInfo to: aSet [
	(aSet includes: aVersionInfo) ifTrue: [^ self].
	aSet add: aVersionInfo.
	(self knownAncestorsOf: aVersionInfo) do:
		[:ea |
		self addAllAncestorsOf: ea to: aSet]
]

{ #category : 'adding' }
MCVersionSorter >> addToCurrentLayer: aVersionInfo [
	| layer |
	layer := layers at: depthIndex.
	(layer includes: aVersionInfo) ifFalse:
		[depths at: aVersionInfo ifPresent:
			[:i |
			i < depthIndex
				ifTrue: [(layers at: i) remove: aVersionInfo]
				ifFalse: [^ false]].
		layer add: aVersionInfo.
		depths at: aVersionInfo put: depthIndex.
		^ true].
	^ false 
]

{ #category : 'adding' }
MCVersionSorter >> addVersionInfo: aVersionInfo [
	roots add: aVersionInfo.
	self registerStepChildrenOf: aVersionInfo seen: Set new
]

{ #category : 'accessing' }
MCVersionSorter >> allAncestorsOf: aVersionInfo [
	| all |
	all := Set new.
	self addAllAncestorsOf: aVersionInfo to: all.
	^ all
]

{ #category : 'initialization' }
MCVersionSorter >> initialize [
	super initialize.
	stepparents := Dictionary new.
	roots := OrderedCollection new.
]

{ #category : 'accessing' }
MCVersionSorter >> knownAncestorsOf: aVersionInfo [
	^ aVersionInfo ancestors, (self stepParentsOf: aVersionInfo) asArray
]

{ #category : 'accessing' }
MCVersionSorter >> layers [
	^ layers
]

{ #category : 'private' }
MCVersionSorter >> popLayer [
	depthIndex := depthIndex - 1
]

{ #category : 'private' }
MCVersionSorter >> processVersionInfo: aVersionInfo [

	(self addToCurrentLayer: aVersionInfo) ifFalse: [ ^ self ].

	self pushLayer.
	(self knownAncestorsOf: aVersionInfo) do: [ :ea | self processVersionInfo: ea ].
	self popLayer
]

{ #category : 'private' }
MCVersionSorter >> pushLayer [
	depthIndex := depthIndex + 1.
	depthIndex > layers size ifTrue: [layers add: OrderedCollection new].
	
]

{ #category : 'private' }
MCVersionSorter >> registerStepChildrenOf: aVersionInfo seen: aSet [
	(aSet includes: aVersionInfo) ifTrue: [^ self].
	aSet add: aVersionInfo.
	aVersionInfo stepChildren do: [:ea | (self stepParentsOf: ea) add: aVersionInfo].
	aVersionInfo ancestors do: [:ea | self registerStepChildrenOf: ea seen: aSet].
]

{ #category : 'accessing' }
MCVersionSorter >> sortedVersionInfos [
	layers := OrderedCollection with: OrderedCollection new.
	depthIndex := 1.
	depths := Dictionary new.
	roots do: [:ea | self processVersionInfo: ea].
	^ layers flatCollect: [:ea | ea]
]

{ #category : 'accessing' }
MCVersionSorter >> stepParentsOf: aVersionInfo [
	^ (stepparents at: aVersionInfo ifAbsentPut: [Set new])
]
