accessing
pretendBitBltSubPixelUnavailableDuring: aBlock	
	"
	For testing/profiling only. 
	
	Answer true if the the subPixel combination rule is available, false otherwise.
	to test :-
	
	bitBltSubPixelAvailable := false. 
	FreeTypeCache current removeAll.
	Smalltalk isMorphic
		ifTrue:[World restoreMorphicDisplay]
		
	"
	| old |
	old := bitBltSubPixelAvailable.
	[bitBltSubPixelAvailable := false.
	FreeTypeCache current removeAll.
	aBlock value.
	] ensure:[
		bitBltSubPixelAvailable := old.
		FreeTypeCache current removeAll.]