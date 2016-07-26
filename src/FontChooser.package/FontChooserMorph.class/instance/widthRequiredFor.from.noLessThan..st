private
widthRequiredFor: aBlock from: aCollection noLessThan: anInteger
	"aBlock - takes each item as an argument, and returns the width it needs"

	^ aCollection inject: anInteger into: [ :max :each | | itemWidth |
		itemWidth := aBlock value: each.
		max max: itemWidth ].