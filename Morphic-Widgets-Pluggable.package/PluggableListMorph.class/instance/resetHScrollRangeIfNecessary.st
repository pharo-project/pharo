scroll cache
resetHScrollRangeIfNecessary

	hScrollRangeCache ifNil: [ ^self deriveHScrollRange ].

	(list isNil or: [list isEmpty]) 
		ifTrue:[^hScrollRangeCache := Array with: 0 with: 0 with: 0 with: 0 with: 0].

"Make a guess as to whether the scroll ranges need updating based on whether the size, first item, or last item of the list has changed"
	(
		(hScrollRangeCache third == list size) and: [
		(hScrollRangeCache fourth == list first) and: [
		(hScrollRangeCache fifth == list last)
	]])
		ifFalse:[self deriveHScrollRange].

