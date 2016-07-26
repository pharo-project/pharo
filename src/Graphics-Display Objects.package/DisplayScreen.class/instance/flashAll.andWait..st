displaying
flashAll: rectangleList andWait: msecs
	"Flash the areas of the screen defined by the given rectangles."

	rectangleList do: [:aRectangle | self reverse: aRectangle].
	self forceDisplayUpdate.
	(Delay forMilliseconds: msecs) wait.
	rectangleList do: [:aRectangle | self reverse: aRectangle].
	self forceDisplayUpdate.
	(Delay forMilliseconds: msecs) wait.
