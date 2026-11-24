"
I render a world embedded in a morph (an AthensWrapWorldMorph).
My display is the morph that holds the world: it gives the world its size.
"
Class {
	#name : 'AthensWrappedWorldRenderer',
	#superclass : 'NullWorldRenderer',
	#instVars : [
		'display'
	],
	#category : 'Athens-Morphic',
	#package : 'Athens-Morphic'
}

{ #category : 'testing' }
AthensWrappedWorldRenderer class >> isApplicableFor: aWorld [
	"Only AthensWrapWorldMorph creates me, for the world it embeds. Never pick me for the main world"

	^ false
]

{ #category : 'accessing' }
AthensWrappedWorldRenderer >> actualScreenSize [

	^ display extent
]

{ #category : 'accessing' }
AthensWrappedWorldRenderer >> display: aWrappedWorldDisplay [

	display := aWrappedWorldDisplay
]

{ #category : 'operations' }
AthensWrappedWorldRenderer >> worldResized [
	"The image's resize action is for the main world, not for an embedded one"

	
]
