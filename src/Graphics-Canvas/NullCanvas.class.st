"
A canvas which ignores all drawing commands.
"
Class {
	#name : #NullCanvas,
	#superclass : #PluggableCanvas,
	#category : 'Graphics-Canvas'
}

{ #category : #drawing-support }
NullCanvas >> clipBy: region during: aBlock [
	"do this in order that timing runs work better"
	aBlock value: self
]

{ #category : #accessing }
NullCanvas >> clipRect [
	^1@1 extent: 99@99
]

{ #category : #copying }
NullCanvas >> copyClipRect: clipRect [
	"who cares what the clipping rectangle is?"
	^self
]

{ #category : #accessing }
NullCanvas >> extent [
	^100@100
]

{ #category : #accessing }
NullCanvas >> form [
	^Form extent: self extent
]

{ #category : #accessing }
NullCanvas >> origin [
	^0@0
]

{ #category : #drawing-support }
NullCanvas >> transformBy: aDisplayTransform clippingTo: aClipRect during: aBlock smoothing: cellSize [
	"do this in order that timing runs work better"
	aBlock value: self
]

{ #category : #drawing-support }
NullCanvas >> translateBy: delta during: aBlock [
	"do this in order that timing runs work better"
	aBlock value: self
]
