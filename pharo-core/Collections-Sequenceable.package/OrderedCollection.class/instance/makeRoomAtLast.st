private
makeRoomAtLast
	"Make some empty slots at the end of the array. If we have more than 50% free space, then just move the elements, so that the last 50% of the slots are free, otherwise add new free slots to the end by growing. Precondition: lastIndex = array size"
	
	| tally newFirstIndex newLastIndex |
	tally := self size.
	tally * 2 >= lastIndex ifTrue: [ ^self growAtLast ].
	tally = 0 ifTrue: [ ^self resetTo: 1 ].
	newLastIndex := lastIndex // 2.
	newFirstIndex := newLastIndex - lastIndex + firstIndex.
	array 
		replaceFrom: newFirstIndex
		to: newLastIndex
		with: array
		startingAt: firstIndex.
	array from: newLastIndex + 1 to: lastIndex put: nil.
	firstIndex := newFirstIndex.
	lastIndex := newLastIndex