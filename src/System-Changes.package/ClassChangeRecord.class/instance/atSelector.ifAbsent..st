method changes
atSelector: selector ifAbsent: absentBlock

	^ (methodChanges at: selector ifAbsent: absentBlock)
		changeType