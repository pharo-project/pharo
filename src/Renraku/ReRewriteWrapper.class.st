"
A wrapper that provides a single interface to produce a new node from the matched node and a maping of wildcard variables.
"
Class {
	#name : #ReRewriteWrapper,
	#superclass : #Object,
	#category : #Renraku-Rules-Rewrite
}

{ #category : #actions }
ReRewriteWrapper >> rewriten: aNode with: aMap [

	^ self subclassResponsibility 
]
