"
A TagMorph is a morph used to define some tags.
It uses a rounded rectancle with a text inside.

self example
"
Class {
	#name : #TagMorphFactory,
	#superclass : #Object,
	#classVars : [
		'color',
		'text'
	],
	#category : 'Versionner-Spec-Browser'
}

{ #category : #example }
TagMorphFactory class >> example [
	(TagMorphFactory newTag: 'development') 
		morph openInHand 
]

{ #category : #'instance creation' }
TagMorphFactory class >> newTag: aText [
	^ self new
		text: aText;
		yourself.
]

{ #category : #accessing }
TagMorphFactory >> color [
	^ color ifNil: [ Color green ]
]

{ #category : #accessing }
TagMorphFactory >> color: aColor [
	color := aColor
]

{ #category : #accessing }
TagMorphFactory >> font [
	^ (LogicalFont 
		familyName: TextStyle defaultFont familyName 
		pointSize: 8)
		emphasis: 1.
]

{ #category : #accessing }
TagMorphFactory >> height [
	^ 12
]

{ #category : #protocol }
TagMorphFactory >> morph [
	| textMorph width |
	textMorph := self textMorph.
	width := textMorph width + 10.
	^ BorderedMorph new
		extent: width @ self height;
		borderWidth: 1;
		color: self color;
		useRoundedCorners;
		addMorphCentered: textMorph;
		yourself
]

{ #category : #accessing }
TagMorphFactory >> text [
	^ text
]

{ #category : #accessing }
TagMorphFactory >> text: aString [
	text := aString
]

{ #category : #private }
TagMorphFactory >> textMorph [
	^ text asMorph 
		color: Color black;
		font:  self font;
		yourself.
]
