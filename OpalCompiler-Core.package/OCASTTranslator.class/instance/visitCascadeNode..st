visitor-double dispatching
visitCascadeNode: aCascadeNode 

	valueTranslator visitNode: aCascadeNode receiver.
	aCascadeNode messages allButLastDo: [:node |
		methodBuilder pushDup.
		effectTranslator visitNode: node.
	].
	self visitNode: aCascadeNode messages last.
