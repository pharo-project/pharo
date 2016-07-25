method changes
findOrMakeMethodChangeAt: selector priorMethod: priorMethod 
	^methodChanges at: selector
		ifAbsentPut: [MethodChangeRecord new]