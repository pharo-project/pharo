accessing
defaultNumberOfVisiblePages: anInteger
	"use it only when the number of pages is 0"
	self size = 0 ifFalse: [ GLMPagerException signal: 'You can change default number of visible pages only when there is no pages' ].
	numberOfVisiblePages := anInteger.