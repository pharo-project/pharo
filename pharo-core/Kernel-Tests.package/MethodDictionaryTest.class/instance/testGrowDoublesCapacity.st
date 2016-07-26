tests - others
testGrowDoublesCapacity
	| methodDictionary oldCapacity |
	methodDictionary := MethodDictionary new.
	oldCapacity := methodDictionary capacity.
	methodDictionary grow.
	self assert: oldCapacity * 2 = methodDictionary capacity.