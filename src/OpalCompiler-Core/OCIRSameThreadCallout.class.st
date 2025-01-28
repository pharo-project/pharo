Class {
	#name : 'OCIRSameThreadCallout',
	#superclass : 'OCIRInstruction',
	#instVars : [
		'functionDefinition'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRSameThreadCallout >> accept: aVisitor [

	^ aVisitor visitSameThreadCallout: self
]

{ #category : 'as yet unclassified' }
OCIRSameThreadCallout >> functionDefinition [

	^ functionDefinition

]

{ #category : 'as yet unclassified' }
OCIRSameThreadCallout >> functionDefinition: aTFExternalFunction [ 

	functionDefinition := aTFExternalFunction 

]
