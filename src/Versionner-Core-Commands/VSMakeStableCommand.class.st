"
I'm a command to declare versions as stable.
"
Class {
	#name : #VSMakeStableCommand,
	#superclass : #VSMakeVersionCommand,
	#category : #'Versionner-Core-Commands'
}

{ #category : #testing }
VSMakeStableCommand >> canBeApplied [
	^ target isStable not and: [ target isBaseline not ]
]

{ #category : #execute }
VSMakeStableCommand >> execute [
	"I expect that target is an MCVersion"
	(MetacelloToolBox configurationNamed: self configurationClass name)
		symbolicVersionMethod: #stable;
		addSymbolicSection: #common version: target versionString;
		commitMethod.

	
	
]

{ #category : #accessing }
VSMakeStableCommand >> symbolicVersion [ 
	^ #stable
]

{ #category : #accessing }
VSMakeStableCommand >> title [
	^ 'make stable version'
]
