Class {
	#name : #ClySubclassN2OfClass1FromP1,
	#superclass : #ClyClass1FromP1,
	#instVars : [
		'subclassN2Var',
		'subclassSameVar'
	],
	#category : #'Calypso-SystemQueries-Tests-P1WithHierarchy'
}

{ #category : #'as yet unclassified' }
ClySubclassN2OfClass1FromP1 class >> classSideMethodFromSubclassN2 [
]

{ #category : #'as yet unclassified' }
ClySubclassN2OfClass1FromP1 >> subclassN2VarReaderMethod [
	^subclassN2Var
]

{ #category : #'as yet unclassified' }
ClySubclassN2OfClass1FromP1 >> subclassN2VarWriterMethod [
	subclassN2Var := #subclassN2VarValue
]

{ #category : #'as yet unclassified' }
ClySubclassN2OfClass1FromP1 >> superclassVar1ReaderMethod [
	^instanceSideVar1 
]

{ #category : #'as yet unclassified' }
ClySubclassN2OfClass1FromP1 >> superclassVar1WriterMethod [
	instanceSideVar1 := #subclassVar1Value 
]
