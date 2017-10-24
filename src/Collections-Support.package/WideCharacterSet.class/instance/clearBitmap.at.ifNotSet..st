private
clearBitmap: aMap at: shortInteger ifNotSet: anAction
	"clear a single bit in aMap.
	shortInteger should be between: 0 and: 16rFFFF"

	| collecIndex oldMapEntry bitMask |
	collecIndex := (shortInteger bitShift: -3) + 1.
	oldMapEntry := aMap at: collecIndex.
	bitMask := 1 bitShift: (shortInteger bitAnd: 7).
	(oldMapEntry bitAnd: bitMask) = 0
		ifTrue: [ ^ anAction value ].
	^ aMap at: collecIndex put: (oldMapEntry bitClear: bitMask)