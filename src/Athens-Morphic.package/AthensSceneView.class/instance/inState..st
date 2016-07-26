state tracking
inState: stateName


	^ (state at: stateName ifAbsent: nil) notNil