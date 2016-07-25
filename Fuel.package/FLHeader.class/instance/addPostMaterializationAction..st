accessing
addPostMaterializationAction: aCleanBlockClosure
	"The closure may (or not) have one parameter and it will be the materialization."
	aCleanBlockClosure isClean ifFalse: [ self error: 'Post materializaton actions have to be clean closures. For more details see method BlocKClosure >> #isClean' ].
	postMaterializationActions add: aCleanBlockClosure 