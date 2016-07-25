drawing
resize
	| extent height |
	firstVisible := 1.
	height := self visibleItemsCount * self class itemHeight.
	pageHeight := self height asInteger.
	self hasMessage ifTrue: [ height := height + self class itemHeight ].
	titleStringMorph ifNotNil: [
		"titleStringMorph width: (titleStringMorph width min: 100)."
		height := height max: titleStringMorph width + 30 ].
	extent := self class itemWidth @ height.
	self extent: extent.