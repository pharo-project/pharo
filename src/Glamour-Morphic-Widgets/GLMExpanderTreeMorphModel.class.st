Class {
	#name : #GLMExpanderTreeMorphModel,
	#superclass : #MorphTreeModel,
	#instVars : [
		'roots',
		'glamourExpander'
	],
	#category : #'Glamour-Morphic-Widgets'
}

{ #category : #accessing }
GLMExpanderTreeMorphModel >> glamourExpander [
	^ glamourExpander
]

{ #category : #accessing }
GLMExpanderTreeMorphModel >> glamourExpander: anObject [
	glamourExpander := anObject
]

{ #category : #accessing }
GLMExpanderTreeMorphModel >> nodeModelFor: anObject [
	^ GLMExpanderLabelNodeModel with: anObject in: self
]

{ #category : #accessing }
GLMExpanderTreeMorphModel >> roots [
"	| filtered |"
	roots ifNil: [ self roots: self glamourExpander panes ].
	^ roots
"	filtered := roots select: [ :each | each shouldBeDisplayed ].
	((self shouldFilterByAmount and: [
		self amountToFilterBy < filtered size]) and: [
			filtered notEmpty]) ifTrue: [
		filtered := filtered first: self amountToFilterBy ].
	^ filtered"
]

{ #category : #accessing }
GLMExpanderTreeMorphModel >> roots: anObjectOrCollection [
	roots := anObjectOrCollection isCollection 
		ifTrue: [ anObjectOrCollection collect: [:each | self nodeModelFor: each ] ]
		ifFalse: [ OrderedCollection with: (self nodeModelFor: anObjectOrCollection) ]
]
