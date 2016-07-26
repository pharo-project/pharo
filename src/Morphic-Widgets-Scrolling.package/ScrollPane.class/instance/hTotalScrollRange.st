geometry
hTotalScrollRange
	"Return the entire scrolling range."
	^ self hUnadjustedScrollRange + self hExtraScrollRange + self hMargin
