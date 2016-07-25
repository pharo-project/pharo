field definition
compileAlias: spec withAccessors: aBool
	"Define all the fields in the receiver.
	Return the newly compiled spec."
	| fieldName fieldType isPointerField externalType |
	fieldName := spec first.
	fieldType := spec second.
	isPointerField := fieldType last = $*.
	fieldType := fieldType copyWithout: $*.
	externalType := ExternalType atomicTypeNamed: fieldType.
	externalType == nil ifTrue:["non-atomic"
		Symbol hasInterned: fieldType ifTrue:[:sym|
			externalType := ExternalType structTypeNamed: sym]].
	externalType == nil ifTrue:[
		Transcript show:'(', fieldType,' is void)'.
		externalType := ExternalType void].
	isPointerField ifTrue:[externalType := externalType asPointerType].
	(fieldName notNil and:[aBool]) ifTrue:[
		self defineAliasAccessorsFor: fieldName
			type: externalType].
	isPointerField 
		ifTrue:[compiledSpec := WordArray with: 
					(ExternalType structureSpec bitOr: ExternalType pointerSpec)]
		ifFalse:[compiledSpec := externalType compiledSpec].
	ExternalType noticeModificationOf: self.
	^compiledSpec