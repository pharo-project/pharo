removing
removeKey: key ifAbsent: errorBlock 
	"The interpreter might be using this MethodDictionary while
	this method is running! Therefore we perform the removal
	in a copy, and then atomically copy that copy"
	
	| copy removedValue |
	copy := self copy.
	removedValue := copy removeDangerouslyKey: key ifAbsent: [^ errorBlock value].
	self copyFrom: copy.
	^ removedValue