preconditions
areFingersCloseToABorder
	"0.5 represents half the screen in percentage"
	| halfScreen fingersCenterTranslatedToScreenCenter |
	halfScreen := 0.5.
	
	fingersCenterTranslatedToScreenCenter := (self centerOfAllFingers - halfScreen) abs.
	
	^ modeActivated or: [ fingersCenterTranslatedToScreenCenter x > (halfScreen - minDistToBorder)
		or: [ fingersCenterTranslatedToScreenCenter y > (halfScreen - minDistToBorder)  ]]