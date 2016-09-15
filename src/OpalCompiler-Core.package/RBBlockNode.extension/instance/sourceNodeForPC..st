*OpalCompiler-Core
sourceNodeForPC: anInteger 
	self methodNode ir.
	(self hasProperty: #ir) ifTrue: [ "FullBlockClosure"
		^(self ir fullBlockInstructionForPC: anInteger) sourceNode ].
	^ self methodNode sourceNodeForPC: anInteger