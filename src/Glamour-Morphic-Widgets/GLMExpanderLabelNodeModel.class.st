Class {
	#name : #GLMExpanderLabelNodeModel,
	#superclass : #MorphTreeNodeModel,
	#instVars : [
		'containerTree'
	],
	#category : #'Glamour-Morphic-Widgets'
}

{ #category : #'instance creation' }
GLMExpanderLabelNodeModel class >> with: anObject in: aTreeModel [
	^ (self with: anObject) containerTree: aTreeModel; yourself
]

{ #category : #callbacks }
GLMExpanderLabelNodeModel >> childrenItems [
	^ OrderedCollection 
		with: ((GLMExpanderContentsNodeModel with: (self->self item)) containerNode: self)
]

{ #category : #accessing }
GLMExpanderLabelNodeModel >> containerTree [
	^ containerTree
]

{ #category : #accessing }
GLMExpanderLabelNodeModel >> containerTree: anObject [
	containerTree := anObject
]

{ #category : #callbacks }
GLMExpanderLabelNodeModel >> contents [
	^ self childrenItems
]

{ #category : #accessing }
GLMExpanderLabelNodeModel >> displayText [
	^ UITheme current 
		newTextIn: World
		text: (self item presentations titleValue ifNil: ['noname']) 
]

{ #category : #callbacks }
GLMExpanderLabelNodeModel >> elementColumn [
	| row |
	row := OrderedCollection with: (self displayText).
	
"	tags:= self containerTree glamourPresentation tagsFor: self item to: #show.
	tagsFilter := self containerTree glamourPresentation tagsFor: self item to: #filter.
	tags withIndexDo:[ :each :index | 
			row addLast: (self buttonForTag: each 
									  filter: (tagsFilter at: index ifAbsentPut:[ each ])) ].
"	^ (UITheme current newRowIn: World for: row) fillStyle: Color veryLightGray
]

{ #category : #accessing }
GLMExpanderLabelNodeModel >> icon [
	^ nil
]
