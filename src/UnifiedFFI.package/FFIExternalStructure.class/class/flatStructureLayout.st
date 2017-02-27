register marshalling
flatStructureLayout
	| result |
	result := FFIExternalStructureFlatLayout new.
	self emitFlatStructureLayoutInto: result.
	^ result