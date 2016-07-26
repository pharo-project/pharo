adding
add: newObject withOccurrences: anInteger
	"Add newObject anInteger times to the receiver. Answer newObject."

	anInteger timesRepeat: [self add: newObject].
	^ newObject