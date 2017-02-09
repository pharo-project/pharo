*OpalCompiler-Core
sourceNodeExecutedForPC: anInteger
	self methodNode ir.
	(self hasProperty: #ir) ifTrue: [ "FullBlockClosure"
		^(self ir fullBlockInstructionForPC: anInteger) sourceNodeExecuted ].
	^ self methodNode sourceNodeExecutedForPC: anInteger