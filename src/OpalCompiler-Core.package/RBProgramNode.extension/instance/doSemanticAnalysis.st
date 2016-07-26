*opalcompiler-core
doSemanticAnalysis
	self methodNode ifNil: [ ^self ].
	^ self methodNode doSemanticAnalysis.
	