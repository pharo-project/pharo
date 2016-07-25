accessing
stepParentsOf: aVersionInfo
	^ (stepparents at: aVersionInfo ifAbsentPut: [Set new])