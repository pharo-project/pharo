Class {
	#name : #ContextTestAssistantB,
	#superclass : #ContextTestAssistantA,
	#category : #'Kernel-Tests-Methods'
}

{ #category : #'as yet unclassified' }
ContextTestAssistantB class >> lookupSymbolInThisContext: aSymbol [
	^ (thisContext lookupSymbol: aSymbol)
]
