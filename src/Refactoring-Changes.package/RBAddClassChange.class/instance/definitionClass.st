private
definitionClass
	^ Smalltalk globals at: (self superclassName ifNil: [ ^ ProtoObject ])