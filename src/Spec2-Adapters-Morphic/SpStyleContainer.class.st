Class {
	#name : #SpStyleContainer,
	#superclass : #SpStyleProperty,
	#instVars : [
		'borderWidth',
		'padding'
	],
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #'ston-core' }
SpStyleContainer class >> stonName [

	^ 'Container'
]

{ #category : #operations }
SpStyleContainer >> applyTo: aMorph [ 

	self borderWidth ifNotNil: [ :aNumber | aMorph borderWidth: aNumber ].
	self padding ifNotNil: [ :aNumber | aMorph cellInset: aNumber ]
]

{ #category : #accessing }
SpStyleContainer >> borderWidth [
	^ borderWidth
]

{ #category : #accessing }
SpStyleContainer >> borderWidth: anObject [
	borderWidth := anObject
]

{ #category : #accessing }
SpStyleContainer >> padding [
	^ padding
]

{ #category : #accessing }
SpStyleContainer >> padding: anObject [
	padding := anObject
]
