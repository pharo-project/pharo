"
I know how to render a ruler link in an Athens canvas.

Note: an instance of me cannot be used to render in more than one canvas.
"
Class {
	#name : #HiLinkRenderer,
	#superclass : #HiRenderer,
	#instVars : [
		'checkpoints',
		'linkBuilder',
		'rulerBuilder'
	],
	#category : #'Hiedra-Rendering'
}

{ #category : #accessing }
HiLinkRenderer >> addCheckpoint: aPoint [
	checkpoints add: aPoint
]

{ #category : #accessing }
HiLinkRenderer >> checkpoints: aCollectionOfPoints [
	checkpoints := aCollectionOfPoints
]

{ #category : #initialization }
HiLinkRenderer >> initialize [
	super initialize.
	
	checkpoints := OrderedCollection new.
]

{ #category : #accessing }
HiLinkRenderer >> linkBuilder: aHiLinkBuilder [ 
	linkBuilder := aHiLinkBuilder
]

{ #category : #accessing }
HiLinkRenderer >> linkModel: aHiLinkBuilder [ 
	self 
		deprecated: 'Use linkBuilder: instead'
		on: 	'17 March 2016' 
		in: 'Pharo-5.0-50571-'.
	^ self linkBuilder: aHiLinkBuilder 
]

{ #category : #accessing }
HiLinkRenderer >> rulerBuilder: aHiRulerBuilder [ 
	rulerBuilder := aHiRulerBuilder
]

{ #category : #accessing }
HiLinkRenderer >> rulerModel: aHiRulerBuilder [ 
	self 
		deprecated: 'Use rulerBuilder: instead'
		on: 	'17 March 2016' 
		in: 'Pharo-5.0-50571-'.
	^ self rulerBuilder: aHiRulerBuilder 
]

{ #category : #accessing }
HiLinkRenderer >> setPaint [
"	| paint |
	paint := athensCanvas surface 
		createLinearGradient: { 0 -> linkBuilder color muchLighter.  1 -> linkBuilder color } 
		start: checkpoints first
		stop: checkpoints last.
"
	(athensCanvas setStrokePaint: linkBuilder color) 
		width: linkBuilder width.
		
	athensCanvas paint 
		dashes: linkBuilder dashes 
		offset: 0
]
