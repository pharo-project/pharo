Class {
	#name : #ContextTestAssistantA,
	#superclass : #Object,
	#classVars : [
		'classVarForTestLookupSymbol'
	],
	#category : #'Kernel-Tests-Methods'
}

{ #category : #'as yet unclassified' }
ContextTestAssistantA class >> classVarForTestLookupSymbol [
	^ classVarForTestLookupSymbol
]

{ #category : #'as yet unclassified' }
ContextTestAssistantA class >> classVarForTestLookupSymbol: aValue [
	classVarForTestLookupSymbol := aValue.
]
