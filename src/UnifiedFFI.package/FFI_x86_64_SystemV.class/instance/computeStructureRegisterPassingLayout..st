default abi
computeStructureRegisterPassingLayout: aStructureClass
	| layout |
	aStructureClass structureSize > self maxStructureSizeToPassInRegisters ifTrue: [ ^ nil ].
	layout := aStructureClass flatStructureLayout sysVAMD64PostProcess.
	^ layout isPassedInMemory ifTrue: [ nil ] ifFalse: [ layout ]