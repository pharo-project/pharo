private
applyFilterInContext: aSpotterContext error: anException
	('[Spotter] Exception in filter <', self class name, '>: ', anException asString) logCr.
	^ self defaultFilteredItems