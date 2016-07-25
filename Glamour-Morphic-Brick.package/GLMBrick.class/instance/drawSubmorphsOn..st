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
			each ifNotNil: [ self flag: 'fix nil case'.
			globalBounds := each isBrick
				ifTrue: [ each globalBounds ]
				ifFalse: [ each fullBounds ].
				
			(area intersects: globalBounds) ifTrue: [
				| zIndex |
				zIndex := each layoutProperties zIndex.
				zIndex = 1 ifTrue: [
					"immediately draw ones with the lowest possible z-index.
					With this approach performance doesn't degrade"
					intersection := area intersect: globalBounds ifNone: (0 @ 0 extent: 0 @ 0).
					canvas := aCanvas copyClipRect: intersection.
					each fullDrawOn: canvas ]
					"if z-index is not 1 we add it to collection, in order to sort and draw as second attempt"
				ifFalse: [ zIndexedBricks add: each ] ] ] ].
		
		"z-index drawing section"
		zIndexedBricks sort: [ :first :second | first layoutProperties zIndex < second layoutProperties zIndex ].
		
		zIndexedBricks do: [ :each |
			| intersection canvas globalBounds |
			each ifNotNil: [ self flag: 'fix nil case'.
			globalBounds := each isBrick
				ifTrue: [ each globalBounds ]
				ifFalse: [ each fullBounds ].
				
			intersection := area intersect: globalBounds ifNone: (0 @ 0 extent: 0 @ 0).
			canvas := aCanvas copyClipRect: intersection.
			each fullDrawOn: canvas ] ]
		