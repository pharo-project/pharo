"
This is a helper class for the TablePresentation.

Instance Variables:
	title	<ProtoObject | PseudoContext>
	computation	<ProtoObject | PseudoContext>
"
Class {
	#name : #GLMTableColumn,
	#superclass : #Object,
	#traits : 'GLMTBlockTags',
	#classTraits : 'GLMTBlockTags classTrait',
	#instVars : [
		'title',
		'computation',
		'width',
		'tagsBlock',
		'tagsFilterBlock',
		'sortBlock',
		'modifiedBlock'
	],
	#category : #'Glamour-Presentations'
}

{ #category : #accessing }
GLMTableColumn >> computation [
	^ computation
]

{ #category : #accessing }
GLMTableColumn >> computation: anObject [
	computation := anObject
]

{ #category : #accessing }
GLMTableColumn >> defaultSortBlockForPresentation: aGlamourPresentation [
	
	^ [ :x :y | 		
			(aGlamourPresentation
				column: self 
				valueFor: x) asString <
			(aGlamourPresentation 
				column: self 
				valueFor: y) asString ] 
]

{ #category : #accessing }
GLMTableColumn >> modifiedBlock [
	^ modifiedBlock
]

{ #category : #accessing }
GLMTableColumn >> modifiedBlock: anObject [
	modifiedBlock := anObject
]

{ #category : #accessing }
GLMTableColumn >> sortBlock [
	^ sortBlock
]

{ #category : #accessing }
GLMTableColumn >> sortBlock: aBlock [
	sortBlock := aBlock
]

{ #category : #accessing }
GLMTableColumn >> tagsBlock [
	^ tagsBlock
]

{ #category : #accessing }
GLMTableColumn >> tagsBlock: aBlock [
	tagsBlock := aBlock
]

{ #category : #accessing }
GLMTableColumn >> tagsFilter: aBlock [
	tagsFilterBlock := aBlock 
]

{ #category : #accessing }
GLMTableColumn >> tagsFilterBlock [
	^ tagsFilterBlock
]

{ #category : #accessing }
GLMTableColumn >> title [
	^ title
]

{ #category : #accessing }
GLMTableColumn >> title: anObject [
	title := anObject
]

{ #category : #accessing }
GLMTableColumn >> width [
	^ width ifNil: [200]
]

{ #category : #accessing }
GLMTableColumn >> width: anObject [
	width := anObject
]
