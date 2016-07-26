private
compileFields: specArray withAccessors: defineBoolean
	| totalSize fieldSpec |
			
	fieldSpec := self fieldSpec.
	totalSize := 0.	
	externalStructureAlignment := 1.
	fieldSpec fieldsAndTypesDo: [ :fieldName :type | 
		defineBoolean ifTrue: [ 
			self defineFieldAccessorsFor: fieldName startingAt: 1 type: type ]. 
		totalSize := totalSize max: type typeSize.
		externalStructureAlignment := externalStructureAlignment max: type typeAlignment  ].
	totalSize := totalSize alignedTo: externalStructureAlignment.
	
	"Real compiled spec is the compiled spec of fields plus a header with structure size and 
	 structure flag"
	"Temporal type to ensure cyclic (pointer) references will work (case of linked lists, 
	 for example). I do not like it, but it works :S"
	compiledSpec := {ExternalType pointerSpec}. 
	"Now I can reconsider it"
	compiledSpec := fieldSpec compileSpec copyWithFirst: (totalSize bitOr: FFIFlagStructure).
	ExternalType noticeModificationOf: self.
	^ compiledSpec