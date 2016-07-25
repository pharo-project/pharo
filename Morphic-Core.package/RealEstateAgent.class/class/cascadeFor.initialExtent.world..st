strategy
cascadeFor: aView initialExtent: initialExtent world: aWorld

	| position allowedArea |
	allowedArea := self maximumUsableAreaInWorld: aWorld.
	position := aWorld currentWindow isMorph 
		ifFalse: [ aWorld currentHand position - 5]
		ifTrue: [ aWorld currentWindow position + 20].
	^ (position extent: initialExtent)
		translatedAndSquishedToBeWithin: allowedArea