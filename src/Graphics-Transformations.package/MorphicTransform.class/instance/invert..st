transformations
invert: aPoint 
	"Transform the given point from local to global coordinates."
	| p3 p2 |
	self isPureTranslation ifTrue: [ ^ aPoint - offset ].
	p3 := aPoint * scale.
	p2 := (p3 x * angle cos + (p3 y * angle sin)) @ (p3 y * angle cos - (p3 x * angle sin)).
	^ p2 - offset