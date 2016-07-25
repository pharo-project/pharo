scaling, rotation
flipBy: direction centerAt: aPoint
	| oldColors newForm |
	oldColors := colors.
	self colors: nil.
	newForm := super flipBy: direction centerAt: aPoint.
	self colors: oldColors.
	newForm colors: oldColors.
	^newForm 