transformations
transform: aPoint 
	"Transform the given point from global to local coordinates."
	| p2 p3 |
	self isPureTranslation ifTrue: [ ^ aPoint + offset ].
	p2 := aPoint + offset.
	p3 := (p2 x * angle cos - (p2 y * angle sin)) @ (p2 y * angle cos + (p2 x * angle sin)) / scale.
	^ p3