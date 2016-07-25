query
allCallsOn: aSymbol 
	"Answer a Collection of all the methods that call on the provided symbol even deeply embedded in literal array."
	"self new allCallsOn: #allCallsOn:"
	
	^ self allReferencesTo: aSymbol