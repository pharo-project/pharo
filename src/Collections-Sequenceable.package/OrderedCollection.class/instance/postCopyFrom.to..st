copying
postCopyFrom: startIndex to: endIndex 
	"finish copying the array in a certain range."

	endIndex < startIndex ifFalse: [
		"Because actual size of the array may be greater than used size,
		postCopyFrom:to: may fail to fail and answer an incorrect result
		if this sanity check were not applied."
		(startIndex between: 1 and: self size) ifFalse: [^SubscriptOutOfBounds signalFor: startIndex lowerBound: (1 min: self size) upperBound: self size in: self].
		(endIndex between: 1 and: self size) ifFalse: [^SubscriptOutOfBounds signalFor: endIndex lowerBound: (1 min: self size) upperBound: self size in: self]].
	
	"Add a protection that lacks in Array>>postcopy"
	array := array copyFrom: startIndex + firstIndex - 1 to: (endIndex max: startIndex - 1) + firstIndex - 1.
	firstIndex := 1.
	lastIndex := array size