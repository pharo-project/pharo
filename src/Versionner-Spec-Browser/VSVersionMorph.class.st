"
A MTVersionMorph is morph used to display a Metacello version.
It display the version string and optional tags (dev, stable, etc.)
"
Class {
	#name : #VSVersionMorph,
	#superclass : #PanelMorph,
	#instVars : [
		'version',
		'tags'
	],
	#category : #Versionner-Spec-Browser
}

{ #category : #'instance creation' }
VSVersionMorph class >> withVersion: anMTVersion [
	^ self new 
		version: anMTVersion ;
		yourself.
]

{ #category : #private }
VSVersionMorph >> addTag: aStringOrSymbol [
	tags add: (TagMorphFactory newTag: aStringOrSymbol asMorph) morph
]

{ #category : #private }
VSVersionMorph >> adoptPaneColor: paneColor [
	"We prevent the adoption of the parent color to be sure to be able to control the final rendered color"
]

{ #category : #private }
VSVersionMorph >> build [	
	version isStable 
		ifTrue: [ self addTag: #stable ].
	version isDevelopment 
		ifTrue: [ self addTag: #development ].	
	self addAllMorphs: tags.
	version isDevelopment
		ifFalse: [  self addMorphFront: version name asMorph ]
]

{ #category : #initialization }
VSVersionMorph >> initialize [
	super initialize.	
	tags := OrderedCollection new.
	self 
		changeTableLayout;
		listDirection: #leftToRight;
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap;
		cellInset: 3;
		color: Color transparent;
		yourself.
	
]

{ #category : #accessing }
VSVersionMorph >> version: anObject [
	version := anObject.
	version ifNotNil: [ self build ].
]
