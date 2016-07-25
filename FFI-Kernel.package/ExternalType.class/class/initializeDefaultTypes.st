class initialization
initializeDefaultTypes
	"ExternalType initialize"
	| type pointerType |
	AtomicTypes = nil ifTrue:[
		"Create new atomic types and setup the dictionaries"
		AtomicTypes := Dictionary new.
		StructTypes := WeakValueDictionary new.
		AtomicTypeNames valuesDo:[:k|
			type := self basicNew.
			pointerType := self basicNew.
			AtomicTypes at: k put: type.
			type setReferencedType: pointerType.
			pointerType setReferencedType: type.
		].
	].
	self initializeAtomicTypes.
	self initializeStructureTypes.
	"AtomicTypes := nil"