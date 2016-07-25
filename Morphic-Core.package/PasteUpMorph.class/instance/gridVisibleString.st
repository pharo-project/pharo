gridding
gridVisibleString
	"Answer a string to be used in a menu offering the opportunity 
	to show or hide the grid"
	^ (self gridVisible
		ifTrue: ['<yes>']
		ifFalse: ['<no>'])
		, 'show grid when gridding' translated