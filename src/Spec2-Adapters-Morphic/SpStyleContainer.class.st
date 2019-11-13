"
I'm a style to control the properties of a container. 
I keep properties like 

- borderColor
- borderWidth
- padding
"
Class {
	#name : #SpStyleContainer,
	#superclass : #SpStyleProperty,
	#instVars : [
		'borderWidth',
		'padding',
		'borderColor'
	],
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #'ston-core' }
SpStyleContainer class >> stonName [

	^ 'Container'
]

{ #category : #operations }
SpStyleContainer >> applyTo: aMorph [ 

	self borderColor ifNotNil: [ :aColor | aMorph borderColor: aColor ].
	self borderWidth ifNotNil: [ :aNumber | aMorph borderWidth: aNumber ].
	self padding ifNotNil: [ :aNumber | aMorph cellInset: aNumber ]
]

{ #category : #accessing }
SpStyleContainer >> borderColor [

	^ borderColor
]

{ #category : #accessing }
SpStyleContainer >> borderColor: aColor [

	borderColor := aColor
]

{ #category : #accessing }
SpStyleContainer >> borderWidth [

	^ borderWidth
]

{ #category : #accessing }
SpStyleContainer >> borderWidth: aNumber [

	borderWidth := aNumber
]

{ #category : #accessing }
SpStyleContainer >> padding [

	^ padding
]

{ #category : #accessing }
SpStyleContainer >> padding: aNumber [

	padding := aNumber
]
