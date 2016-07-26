private
tryMatch
	"Match thyself against the current stream."

	| oldMarkerPositions |
	oldMarkerPositions := markerPositions.
	markerPositions := Array new: markerCount.
	1 to: markerCount do: [ :i | markerPositions at: i put: OrderedCollection new ].
	lastResult := startOptimizer isNil
		ifTrue: [ matcher matchAgainst: self]
		ifFalse: [ (startOptimizer canStartMatch: stream peek in: self) and: [ matcher matchAgainst: self ] ].
	"check for duplicates"
	(lastResult not or: [ oldMarkerPositions isNil or: [ oldMarkerPositions size ~= markerPositions size ] ])
		ifTrue: [ ^ lastResult ].
	oldMarkerPositions with: markerPositions do: [ :oldPos :newPos |
		oldPos size = newPos size 
			ifFalse: [ ^ lastResult ].
		oldPos with: newPos do: [ :old :new |
			old = new
				ifFalse: [ ^ lastResult ] ] ].
	"this is a duplicate"
	^ lastResult := false