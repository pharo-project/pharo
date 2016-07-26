*System-Caching
memoized
	"Returns a memoized version of a one argument block (so values that were
	  already calculated will not be computed a second time but returned from a cache)."
	
	^ self memoizedUsing:  Dictionary new.