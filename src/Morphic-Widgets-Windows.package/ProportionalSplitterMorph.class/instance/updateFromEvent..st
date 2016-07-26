event handling
updateFromEvent: anEvent
	"Update the splitter and attached morph positions from the mouse event.
	Take into account the mouse down offset."
	
	| pNew pOld delta selfTop selfBottom selfLeft selfRight|
	pNew := anEvent cursorPoint - lastMouse second.
	pOld := lastMouse first - lastMouse second.
	delta := splitsTopAndBottom
		ifTrue: [0 @ ((self normalizedY: pNew y) - pOld y)]
		ifFalse: [(self normalizedX: pNew x) - pOld x @ 0].
	lastMouse at: 1 put: (splitsTopAndBottom
		ifTrue: [pNew x @ (self normalizedY: pNew y) + lastMouse second]
		ifFalse: [(self normalizedX: pNew x) @ pNew y + lastMouse second]).
	leftOrTop do: [:each | | firstRight firstBottom | 
		firstRight := each layoutFrame rightOffset
					ifNil: [0].
		firstBottom := each layoutFrame bottomOffset
					ifNil: [0].
		each layoutFrame rightOffset: firstRight + delta x.
		each layoutFrame bottomOffset: firstBottom + delta y.
		(each layoutFrame leftFraction = each layoutFrame rightFraction and: [
				each layoutFrame leftFraction ~= 0]) "manual splitter"
			ifTrue: [each layoutFrame leftOffset: (each layoutFrame leftOffset ifNil: [0]) + delta x].
		(each layoutFrame topFraction = each layoutFrame bottomFraction and: [
				each layoutFrame topFraction ~= 0]) "manual splitter"
			ifTrue: [each layoutFrame topOffset: (each layoutFrame topOffset ifNil: [0]) + delta y]].
	rightOrBottom do: [:each | | secondLeft secondTop | 
		secondLeft := each layoutFrame leftOffset
					ifNil: [0].
		secondTop := each layoutFrame topOffset
					ifNil: [0].
		each layoutFrame leftOffset: secondLeft + delta x.
		each layoutFrame topOffset: secondTop + delta y.
		(each layoutFrame leftFraction = each layoutFrame rightFraction and: [
				each layoutFrame rightFraction ~= 1]) "manual splitter"
			ifTrue: [each layoutFrame rightOffset: (each layoutFrame rightOffset ifNil: [0]) + delta x].
		(each layoutFrame topFraction = each layoutFrame bottomFraction and: [
				each layoutFrame bottomFraction ~= 1]) "manual splitter"
			ifTrue: [each layoutFrame bottomOffset: (each layoutFrame bottomOffset ifNil: [0]) + delta y]].
	selfTop := self layoutFrame topOffset ifNil: [0].
	selfBottom := self layoutFrame bottomOffset ifNil: [0].
	selfLeft := self layoutFrame leftOffset ifNil: [0].
	selfRight := self layoutFrame rightOffset ifNil: [0].
	self layoutFrame topOffset: selfTop + delta y.
	self layoutFrame bottomOffset: selfBottom + delta y.
	self layoutFrame leftOffset: selfLeft + delta x.
	self layoutFrame rightOffset: selfRight + delta x.
	self owner layoutChanged