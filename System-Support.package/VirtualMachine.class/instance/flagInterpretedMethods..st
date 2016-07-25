attributes
flagInterpretedMethods: aBoolean
	"The Cog VM can be instructed to set the flag bit of CompiledMethods that it executes but will only interpret.  This can be used e.g. to profile startup. See CompiledMethod>>#flag & CompiledMethod>>#clearFlag. This flag persists across snapshots, stored in the image header."
	
	self parameterAt: 48 put: ((self parameterAt: 48) bitClear: 2) + (aBoolean ifTrue: [2] ifFalse: [0])