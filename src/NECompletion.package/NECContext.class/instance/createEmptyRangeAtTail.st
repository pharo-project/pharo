private-compute-index
createEmptyRangeAtTail
	| previous |
	previous := ranges last.
	ranges add: (SHRange 
			start: previous end + 1
			end: source size
			type: #empty).
	^ ranges size