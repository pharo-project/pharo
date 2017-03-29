morph-drawing
drawSubmorphsOn: aCanvas
	| area zIndexedBricks |
	
	self submorphs isEmpty ifTrue: [ ^ self ].

	"make sure my children will not go away of my inner bounds (we take care about paddings)"
	area := aCanvas clipRect intersect: self globalInnerBounds ifNone: (0 @ 0 extent: 0 @ 0).
	"do smart drawing logic here"
	area width = 0 | (area height = 0) ifTrue: [ 
			"draw nothing"
			^ self ].

	zIndexedBricks := OrderedCollection new.
	
	self subbricks
		do: [ :each | 
			| intersection canvas globalBounds |
			each ifNotNil: [
			globalBounds := each globalClippingBounds.

			(area intersects: globalBounds) ifTrue: [
				| zIndex |
				zIndex := each layoutProperties zIndex brickValue: each.
				zIndex = 1 ifTrue: [
					"immediately draw ones with the lowest possible z-index.
					With this approach performance doesn't degrade"
					intersection := area intersect: globalBounds ifNone: (0 @ 0 extent: 0 @ 0).
					canvas := aCanvas copyClipRect: intersection.
					each fullDrawOn: canvas ]
					"if z-index is not 1 we add it to collection, in order to sort and draw as second attempt"
				ifFalse: [ zIndexedBricks add: each ] ] ] ].
		
		zIndexedBricks sort: [ :first :second | (first layoutProperties zIndex brickValue: first) < (second layoutProperties zIndex brickValue: second) ].
		
		zIndexedBricks do: [ :each |
			| intersection canvas globalBounds |
			each ifNotNil: [
			globalBounds := each globalClippingBounds.

			intersection := area intersect: globalBounds ifNone: (0 @ 0 extent: 0 @ 0).
			canvas := aCanvas copyClipRect: intersection.
			each fullDrawOn: canvas ] ]
		