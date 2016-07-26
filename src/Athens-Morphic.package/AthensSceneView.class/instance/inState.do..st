state tracking
inState: stateName do: aBlock


	^ (state at: stateName ifAbsent: nil) ifNotNil: aBlock 