private
filter: anEntry ifAccepted: acceptedBlock ifNotAccepted: notAcceptedBlock
	^ (self andFilter accepts: anEntry)
		ifTrue: acceptedBlock
		ifFalse: notAcceptedBlock