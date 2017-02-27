register marshalling
emitFlatStructureLayoutInto: flatStructureLayout
	flatStructureLayout alignTo: self structureAlignment.
	self fieldSpec fieldsAndTypesDo: [ :field :type |
		type emitFlatStructureLayoutFieldInto: flatStructureLayout
	].
	flatStructureLayout alignTo: self structureAlignment.