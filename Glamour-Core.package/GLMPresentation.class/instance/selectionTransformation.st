transmitting
selectionTransformation
	"this is used for transforming the object before updating the selection"
	^ self selectionTransmission transformation ifNil: [#yourself]