clipping
clipBy: aRectangle during: aBlock

	| oldRect transformedClip|
	transformedClip := (pathTransform transform:aRectangle origin) corner: (pathTransform transform:aRectangle corner).
	oldRect := engine clipRect.
	engine clipRect: (oldRect intersect: transformedClip).

	aBlock ensure: [ engine clipRect: oldRect ]