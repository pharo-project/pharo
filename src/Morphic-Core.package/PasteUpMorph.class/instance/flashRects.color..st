world state
flashRects: rectangleList color: aColor
	"For testing. Flashes the given list of rectangles on the Display so you can watch incremental redisplay at work."
	"Details: Uses two reverses so that the display is restored to its original state. This is necessary when in deferred update mode."

	| blt |
	blt := (BitBlt toForm: Display)
		sourceForm: nil;
		sourceOrigin: 0@0;
		clipRect: self viewBox;
		combinationRule: Form reverse.
	rectangleList do: [:r | | screenRect |
		screenRect := r translateBy: self viewBox origin.
		blt destRect: screenRect; copyBits.
		Display forceToScreen: screenRect; forceDisplayUpdate.
		(Delay forMilliseconds: 15) wait.
		blt destRect: screenRect; copyBits.
		Display forceToScreen: screenRect; forceDisplayUpdate].
