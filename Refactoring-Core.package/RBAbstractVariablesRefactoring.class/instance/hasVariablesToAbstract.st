testing
hasVariablesToAbstract
	^ instVarReaders notEmpty or: [ instVarWriters notEmpty or: [ classVarReaders notEmpty or: [ classVarWriters notEmpty ] ] ]