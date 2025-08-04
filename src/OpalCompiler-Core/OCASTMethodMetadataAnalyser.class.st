"
This visitor analyses the AST and then adds annotations that will end up being set on the
compild method.

One example are methods that contains message send that is a #halt (and any of it's variations)
"
Class {
	#name : 'OCASTMethodMetadataAnalyser',
	#superclass : 'OCProgramNodeVisitor',
	#category : 'OpalCompiler-Core-Semantics',
	#package : 'OpalCompiler-Core',
	#tag : 'Semantics'
}

{ #category : 'visiting' }
OCASTMethodMetadataAnalyser >> visitMessageNode: aNode [

	super visitMessageNode: aNode.
	aNode isHaltNode ifTrue: [
		aNode methodNode methodPropertyAt: #containsHalt put: true ]
]
