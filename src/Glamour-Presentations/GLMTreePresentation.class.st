"
A GLMTreePresentation  shows the given elements within a tree. The #children block is used to compute the children of an elements. It does not guard against infinite recursion.


Instance Variables
	allExpanded:		<Object>
	children:		<Object>
	expandLevel:		<Object>
	rootsExpanded:		<Object>
	tagsBlock:		<Object>
	tagsFilterBlock:		<Object>

allExpanded
	- xxxxx

children
	- xxxxx

expandLevel
	- xxxxx

rootsExpanded
	- xxxxx

tagsBlock
	- xxxxx

tagsFilterBlock
	- xxxxx

"
Class {
	#name : #GLMTreePresentation,
	#superclass : #GLMListingPresentation,
	#traits : 'GLMTBlockTags',
	#classTraits : 'GLMTBlockTags classTrait',
	#instVars : [
		'children',
		'rootsExpanded',
		'allExpanded',
		'expandLevel',
		'tagsBlock',
		'tagsFilterBlock'
	],
	#category : #'Glamour-Presentations'
}

{ #category : #validation }
GLMTreePresentation >> allDisplayedDo: aBlock [
	self displayValue do: [ :eachRoot | 
		self withChildrenOf: eachRoot do: aBlock ]
]

{ #category : #accessing }
GLMTreePresentation >> allExpanded [
	self shouldAllExpand: true
]

{ #category : #accessing }
GLMTreePresentation >> children [ 
	 
	^children
]

{ #category : #accessing }
GLMTreePresentation >> children: anObject [ 
	 
	children := anObject
]

{ #category : #accessing }
GLMTreePresentation >> childrenValueOf: anObject [
	^ self children glamourValue: (anObject asGlamorousMultiValue, self entity asGlamorousMultiValue)
]

{ #category : #accessing }
GLMTreePresentation >> childrenValueOf: anObject atLevel: anInteger [
	^ self children glamourValue: (
		anObject asGlamorousMultiValue, 
		self entity asGlamorousMultiValue,
		anInteger asGlamorousMultiValue,
		self asGlamorousMultiValue)
]

{ #category : #validation }
GLMTreePresentation >> defaultValidate: anObject on: aPort [

	"we only allow objects that are in the display value"

	aPort name == #selection
		ifFalse: [ ^ true ].
	self entity ifNil: [ ^ false ].
	self
		allDisplayedDo: [ :each | 
			each = anObject
				ifTrue: [ ^ true ]
			].
	^ false
]

{ #category : #accessing }
GLMTreePresentation >> expandLevel [
	^ expandLevel
]

{ #category : #rendering }
GLMTreePresentation >> renderGlamorouslyOn: aRenderer [	
	self registerAnnouncements.
	^ aRenderer renderTreePresentation: self
]

{ #category : #accessing }
GLMTreePresentation >> rootsExpanded [
	self shouldRootsExpand: true
]

{ #category : #accessing }
GLMTreePresentation >> shouldAllExpand [
	^ allExpanded ifNil: [false]
]

{ #category : #accessing }
GLMTreePresentation >> shouldAllExpand: anObject [
	allExpanded := anObject
]

{ #category : #accessing }
GLMTreePresentation >> shouldExpandToLevel [
	^ expandLevel isNil not
]

{ #category : #accessing }
GLMTreePresentation >> shouldExpandToLevel: level [
	^ expandLevel := level
]

{ #category : #accessing }
GLMTreePresentation >> shouldRootsExpand [
	^ rootsExpanded ifNil: [false]
]

{ #category : #accessing }
GLMTreePresentation >> shouldRootsExpand: anObject [
	rootsExpanded := anObject
]

{ #category : #accessing }
GLMTreePresentation >> tagsBlock [
	^ tagsBlock
]

{ #category : #scripting }
GLMTreePresentation >> tagsBlock: anObject [
	tagsBlock := anObject
]

{ #category : #scripting }
GLMTreePresentation >> tagsFilter: aBlock [
	tagsFilterBlock := aBlock 
]

{ #category : #accessing }
GLMTreePresentation >> tagsFilterBlock [
	^ tagsFilterBlock
]

{ #category : #validation }
GLMTreePresentation >> withChildrenOf: anObject do: aBlock [
	aBlock value: anObject.
	(self childrenValueOf: anObject) do: [ :each |
		self withChildrenOf: each do: aBlock ]
]
