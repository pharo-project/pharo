"
I'm a generic command for declare versions as stable or development.
"
Class {
	#name : #VSMakeVersionCommand,
	#superclass : #VSVersionCommand,
	#category : #Versionner-Core-Commands
}

{ #category : #execute }
VSMakeVersionCommand >> execute [
	"I expect that target is an MCVersion"
	(MetacelloToolBox configurationNamed: self configurationClass name)
		symbolicVersionMethod: self symbolicVersion;
		addSymbolicSection: #common version: target versionString;
		commitMethod.

	
	
]

{ #category : #accessing }
VSMakeVersionCommand >> symbolicVersion [
	self subclassResponsibility

	
	
]
