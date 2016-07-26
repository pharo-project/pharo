accessing
value: aNumber
	|testWidth|
	value := aNumber.
	testWidth := self completedWidth.
	testWidth ~= cachedWidth ifTrue: [  
		cachedWidth := testWidth.
		self changed]