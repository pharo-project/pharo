updating screen
updateRectangles: aCollectionOfRectangles
	aCollectionOfRectangles do: [ :rectangle | self updateRectangle: rectangle ]