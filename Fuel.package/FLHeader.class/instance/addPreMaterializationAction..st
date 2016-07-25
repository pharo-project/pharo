accessing
addPreMaterializationAction: aCleanBlockClosure
	aCleanBlockClosure isClean ifFalse: [ self error: 'Pre materializaton actions have to be clean closures. For more details see method BlocKClosure >> #isClean' ].
	preMaterializationActions add: aCleanBlockClosure 