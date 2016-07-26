*Monticello-OldDataStreamCompatibility
readReference
	"Read the contents of an object reference. (Cf. outputReference:)  File is not now positioned at this object."
	| referencePosition |
	^ (referencePosition := (byteStream nextNumber: 4)) = self vacantRef	"relative"
		ifTrue:  [nil]
		ifFalse: [self objectAt: referencePosition]		"relative pos"