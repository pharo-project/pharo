Class {
	#name : #GLMPager,
	#superclass : #GLMFinder,
	#instVars : [
		'scrollButtonIconRenderBlock',
		'minimumPageNumberForScrollbar'
	],
	#category : 'Glamour-Morphic-Pager-Glamour'
}

{ #category : #accessing }
GLMPager class >> extensionsPragma [
	^ #gtInspectorPresentationOrder:
]

{ #category : #accessing }
GLMPager >> minimumPageNumberForScrollbar [

	^ minimumPageNumberForScrollbar
]

{ #category : #accessing }
GLMPager >> minimumPageNumberForScrollbar: aNumber [

	minimumPageNumberForScrollbar := aNumber
]

{ #category : #rendering }
GLMPager >> renderGlamorouslyOn: aRenderer [

	^ aRenderer renderPager: self
]

{ #category : #accessing }
GLMPager >> scrollButtonIconRenderBlock [

	^ scrollButtonIconRenderBlock
]

{ #category : #accessing }
GLMPager >> scrollButtonIconRenderBlock: aBlock [

	scrollButtonIconRenderBlock := aBlock
]

{ #category : #testing }
GLMPager >> shouldDisplayPresentationCreatedBy: aCollection [ 
	^ true
]
