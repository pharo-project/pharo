*ston-core
fromSton: stonReader
	"Decode non-variable classes from a map of their instance variables and values.
	Override to customize and add a matching #toSton: (see implementors)."
	
	self class isVariable 
		ifTrue: [
			stonReader error: 'custom #fromSton: implementation needed for variable/indexable class' ]
		ifFalse: [
			stonReader parseNamedInstVarsFor: self ]