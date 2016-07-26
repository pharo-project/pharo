analyzing
referencesOf: aRectangle do: aBlock

	aBlock value: aRectangle origin x.
	aBlock value: aRectangle origin y.
	aBlock value: aRectangle corner x.
	aBlock value: aRectangle corner y.