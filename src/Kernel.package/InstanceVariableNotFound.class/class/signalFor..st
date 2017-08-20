signalling
signalFor: aString
	"Create and signal a NoInstanceVariableNAmed exception for aString in the default receiver."
	
	^ self new
		instVarName: aString;
		signal