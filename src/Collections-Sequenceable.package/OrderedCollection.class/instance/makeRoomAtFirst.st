private
makeRoomAtFirst
	"Make some empty slots at the front of the array. If we have more than 50% free space, then just move the elements, so that the first 50% of the slots are free, otherwise add new free slots to the front by growing. Precondition: firstIndex = 1"
	
	| tally newFirstIndex newLastIndex |
	tally := self size.
	tally * 2 >= array size ifTrue: [ ^self growAtFirst ].
	tally = 0 ifTrue: [ ^self resetTo: array size + 1 ].
	newFirstIndex := array size // 2 + 1.
	newLastIndex := newFirstIndex - firstIndex + lastIndex.
	0 to: tally - 1 do: [ :offset |
		array at: newLastIndex - offset put: (array at: lastIndex - offset) ].
	array from: firstIndex to: newFirstIndex - 1 put: nil.
	firstIndex := newFirstIndex.
	lastIndex := newLastIndex