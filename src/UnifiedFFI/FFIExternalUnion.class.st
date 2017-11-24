"
I'm an extension of FFIExternalStructure.
I put all of my fields at the same offset, which is zero.
"
Class {
	#name : #FFIExternalUnion,
	#superclass : #FFIExternalStructure,
	#category : #'UnifiedFFI-Objects'
}

{ #category : #private }
FFIExternalUnion class >> compileFields: specArray withAccessors: defineBoolean [
	| totalSize fieldSpec |
			
	fieldSpec := self fieldSpec.
	totalSize := 0.	
	externalStructureAlignment := 1.
	fieldSpec fieldsAndTypesDo: [ :fieldName :type | 
		(defineBoolean and: [ fieldName isSymbol ]) ifTrue: [ 
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
]

{ #category : #'register marshalling' }
FFIExternalUnion class >> emitFlatStructureLayoutInto: flatStructureLayout [
	flatStructureLayout addMemoryFieldSize: self structureSize alignment: self structureAlignment
]
