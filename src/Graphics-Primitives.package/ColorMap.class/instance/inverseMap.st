accessing
inverseMap
	"Return the inverse map of the receiver"
	| newMasks newShifts |
	colors ifNotNil: [ ^ self error: 'Not yet implemented' ].
	newMasks := (Array new: 4) writeStream.
	newShifts := (Array new: 4) writeStream.
	masks 
		with: shifts
		do: 
			[ :mask :shift | 
			newMasks nextPut: (mask bitShift: shift).
			newShifts nextPut: shift negated ].
	^ ColorMap 
		shifts: newShifts contents
		masks: newMasks contents