instance creation
for: aRBClass fromMethod: aCompiledMethod andSelector: aSymbol 
	^(self new)
		modelClass: aRBClass;
		method: aCompiledMethod;
		selector: aSymbol;
		yourself