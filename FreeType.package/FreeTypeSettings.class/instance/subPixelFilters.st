accessing
subPixelFilters
	^subPixelFilters ifNil:[subPixelFilters := self subPixelFiltersFromRatios: self defaultSubPixelFilterRatios]