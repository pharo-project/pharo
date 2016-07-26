accessing
clipByX1: x1 y1: y1 x2: x2 y2: y2 
	| right bottom |
	right := clipX + clipWidth.
	bottom := clipY + clipHeight.
	x1 > clipX ifTrue: [ clipX := x1 ].
	y1 > clipY ifTrue: [ clipY := y1 ].
	x2 < right ifTrue: [ right := x2 ].
	y2 < bottom ifTrue: [ bottom := y2 ].
	clipWidth := right - clipX.
	clipHeight := bottom - clipY.
	clipWidth < 0 ifTrue: [ clipWidth := 0 ].
	clipHeight < 0 ifTrue: [ clipHeight := 0 ]