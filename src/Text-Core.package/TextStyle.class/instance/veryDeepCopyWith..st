copying
veryDeepCopyWith: deepCopier 
	"All inst vars are meant to be shared"

	self == #veryDeepCopyWith:.	"to satisfy checkVariables"
	^deepCopier references at: self ifAbsentPut: [self shallowCopy]	"remember"