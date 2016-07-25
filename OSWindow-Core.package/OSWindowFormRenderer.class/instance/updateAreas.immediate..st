morphic integration
updateAreas: allDamage immediate: forceToScreen
	"Force all the damage rects to the screen."
	allDamage do: [ :r| self updateRectangle: r ].