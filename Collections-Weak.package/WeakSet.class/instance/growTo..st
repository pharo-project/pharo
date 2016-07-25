private
growTo: anInteger
	"Grow the elements array and reinsert the old elements"

	| oldElements |
	oldElements := array.
	array := WeakArray new: anInteger withAll: flag.
	self noCheckNoGrowFillFrom: oldElements