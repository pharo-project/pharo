copying
copyFrom: startIndex to: endIndex 
	"Answer a copy of the receiver that contains elements from position
	startIndex to endIndex."

	^self shallowCopy postCopyFrom: startIndex to: endIndex