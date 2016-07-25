protocol
at: key ifAbsentPut: aBlock 

	^ value at: key ifAbsent: [self at: key put: aBlock value]