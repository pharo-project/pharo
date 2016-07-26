private
growTo: anInteger
	"Grow the elements array and reinsert the old elements"
	
	| oldElements |
	oldElements := array.
	array := Array new: anInteger.
	self noCheckNoGrowFillFrom: oldElements