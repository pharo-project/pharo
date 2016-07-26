*Slot
classLayout
	^ layout
		ifNil: [ 
			| superLayout scope |
			superLayout := superclass classLayout.
			scope := superLayout slotScope extend.
			layout := superLayout class extending: superLayout scope: scope host: self ]