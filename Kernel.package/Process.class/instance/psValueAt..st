process specific
psValueAt: index
	"Answer a process-specific value at given index, or nil if value at given index is not defined"
	
	"NOTE: this method are PRIVATE. Do not use it directly, instead use ProcessSpecificVariable (or its subclasses) "
	env ifNil: [ ^ nil ].
	^ env at: index ifAbsent: nil