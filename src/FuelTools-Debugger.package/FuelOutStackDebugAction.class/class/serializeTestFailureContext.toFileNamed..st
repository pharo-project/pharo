serialization
serializeTestFailureContext: aContext toFileNamed: aFilename
	| serializer |
	
	serializer := FLSerializer newDefault.
	self encodeDebugInformationOn: serializer. 
	serializer addPostMaterializationAction: [ :materialization | 
		Smalltalk tools debugger
			openOn: Processor activeProcess 
			context: materialization root 
			label: 'External stack' 
			contents: nil 
			fullView: false ].
		
	serializer
		" use the sender context, generally the current context is not interesting"
		serialize: aContext
		toFileNamed: aFilename