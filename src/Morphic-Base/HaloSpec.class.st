"
Sets spec's for how handles are layed out in a halo.
"
Class {
	#name : #HaloSpec,
	#superclass : #Object,
	#instVars : [
		'addHandleSelector',
		'horizontalPlacement',
		'verticalPlacement',
		'color',
		'iconSymbol'
	],
	#category : #'Morphic-Base-Widgets'
}

{ #category : #actions }
HaloSpec >> addHandleSelector [
	^ addHandleSelector
]

{ #category : #accessing }
HaloSpec >> color [
	^ color
]

{ #category : #accessing }
HaloSpec >> horizontalPlacement [
	^ horizontalPlacement
]

{ #category : #setter }
HaloSpec >> horizontalPlacement: hp verticalPlacement: vp color: col iconSymbol: is addHandleSelector: sel [
	horizontalPlacement := hp.
	verticalPlacement := vp.
	color:= col.
	iconSymbol := is asSymbol.
	addHandleSelector := sel
]

{ #category : #accessing }
HaloSpec >> iconSymbol [
	^ iconSymbol
]

{ #category : #printing }
HaloSpec >> printOn: aStream [
	"Add a textual printout representing the receiver to a stream"

	super printOn: aStream.
	aStream nextPutAll: ' (', addHandleSelector asString, ' ', iconSymbol asString, ')'
]

{ #category : #accessing }
HaloSpec >> verticalPlacement [
	^ verticalPlacement
]
