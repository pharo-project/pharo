copying
forceTo: length paddingWith: elem
	"Force the length of the collection to length, padding
	if necessary with elem.  Note that this makes a copy."

	| newCollection |
	newCollection := self species new: length withAll: elem.
	newCollection replaceFrom: 1 to: (self size min: length) with: self startingAt: 1.
	^ newCollection