Extension { #name : #RubPluggableTextFieldMorph }

{ #category : #'*Athens-Morphic' }
RubPluggableTextFieldMorph >> drawOnAthensCanvas: anAthensCanvas [
	"Eventually draw the helpText"
	super drawOnAthensCanvas: anAthensCanvas.
	entryCompletion
		ifNotNil: [entryCompletion closeIfNotNeeded: self]
]
