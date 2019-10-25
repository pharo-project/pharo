"
I'm a style to control how a widget is drawn. 
I keep properties like 

- color 
- backgroundColor
"
Class {
	#name : #SpStyleDraw,
	#superclass : #SpStyleProperty,
	#instVars : [
		'color',
		'backgroundColor'
	],
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #'ston-core' }
SpStyleDraw class >> stonName [

	^ 'Draw'
]

{ #category : #operations }
SpStyleDraw >> applyTo: aMorph [ 

	self color ifNotNil: [ :aColor | aMorph color: aColor ].
	self backgroundColor ifNotNil: [ :aColor | aMorph backgroundColor: aColor ]
]

{ #category : #accessing }
SpStyleDraw >> backgroundColor [
	^ backgroundColor
]

{ #category : #accessing }
SpStyleDraw >> backgroundColor: anObject [
	backgroundColor := anObject
]

{ #category : #accessing }
SpStyleDraw >> color [
	^ color
]

{ #category : #accessing }
SpStyleDraw >> color: anObject [
	color := anObject
]
