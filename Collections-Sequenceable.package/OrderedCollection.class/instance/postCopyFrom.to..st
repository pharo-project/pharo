copying
postCopyFrom: startIndex to: endIndex 
	"finish copying the array in a certain range."

	endIndex < startIndex ifFalse: [
		"Because actual size of the array may be greater than used size,
		postCopyFrom:to: may fail to fail and answer an incorrect result
		if this sanity check were not applied"
		(startIndex between: 1 and: self size) ifFalse: [^self error: 'startIndex is out of bounds'].
		(endIndex between: 1 and: self size) ifFalse: [^self error: 'endIndex is out of bounds']].
	
	"Add a protection that lacks in Array>>postcopy"
	array := array copyFrom: startIndex + firstIndex - 1 to: (endIndex max: startIndex - 1) + firstIndex - 1.
	firstIndex := 1.
	lastIndex := array size