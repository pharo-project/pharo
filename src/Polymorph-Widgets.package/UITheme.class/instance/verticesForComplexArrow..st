scrollbars
verticesForComplexArrow: aRectangle 
	"PRIVATE - answer a collection of vertices to draw a complex arrow"

	|r vertices aux b c|
	r := aRectangle origin extent: aRectangle width asPoint.
	aux := (r width / 5) rounded.
	b := (aux * aux * 2) sqrt.
	c := (b * b * 2) sqrt rounded.
	vertices := OrderedCollection new.
	vertices add: r bottomLeft - (0 @ aux).
	vertices add: r topCenter + (0 @ aux).
	vertices add: r bottomRight - (0 @ aux).
	vertices add: r bottomRight - (aux @ 0).
	vertices add: r topCenter + (0 @ (aux + c)).
	vertices add: r bottomLeft + (aux @ 0).
	^vertices