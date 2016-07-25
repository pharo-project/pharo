accessing
actualNumberOfVisiblePages
	"returns actual number of visible pages based on first and last visible page indexes"

	(firstVisiblePageIndex = lastVisiblePageIndex) & (firstVisiblePageIndex = 0) ifTrue: [ ^ 0 ].
	^ lastVisiblePageIndex - firstVisiblePageIndex + 1.