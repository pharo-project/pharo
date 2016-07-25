*opalcompiler-core
methodCompilationContextOrNil
	^ self methodNode ifNotNil: [ :node | node compilationContext ]