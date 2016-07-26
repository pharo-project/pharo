accessing
setSubPixelFilter: ratiosArray
	"Set the subPixelFilters from ratiosArray.
	the ratiosArray can specify the red, green, and blue filter ratios separately.
	e.g. #((1 3 5 3 1) (1 4 7 4 1) (1 2 3 2 1))
	or, as single set of ratios e.g. #(1 3 5 3 1)" 
	
	| validArray newFilters |
	validArray := ratiosArray.
	(ratiosArray size = 5)
		ifTrue:[validArray := {ratiosArray. ratiosArray. ratiosArray}].
	newFilters := self subPixelFiltersFromRatios: validArray. 
	(newFilters = subPixelFilters)
		ifFalse:[
			subPixelFilters := newFilters.
			FreeTypeCache current removeAllForType: FreeTypeCacheGlyphLCD.
			World restoreMorphicDisplay]