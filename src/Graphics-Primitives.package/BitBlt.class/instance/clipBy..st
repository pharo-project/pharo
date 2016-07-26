accessing
clipBy: aRectangle 
	| aPoint right bottom |
	right := clipX + clipWidth.
	bottom := clipY + clipHeight.
	aPoint := aRectangle origin.
	aPoint x > clipX ifTrue: [ clipX := aPoint x ].
	aPoint y > clipY ifTrue: [ clipY := aPoint y ].
	aPoint := aRectangle corner.
	aPoint x < right ifTrue: [ right := aPoint x ].
	aPoint y < bottom ifTrue: [ bottom := aPoint y ].
	clipWidth := right - clipX.
	clipHeight := bottom - clipY.
	clipWidth < 0 ifTrue: [ clipWidth := 0 ].
	clipHeight < 0 ifTrue: [ clipHeight := 0 ]